#!/usr/bin/env python3
"""Full-board stochastic optimizer for Eternity II.

This solver keeps every piece placed at all times.  The hard constraints
outside the puzzle are never violated: corner pieces stay on corners, edge
pieces stay on edges with their gray side outward, and interior pieces stay
inside.  The search then optimizes the 480 internal adjacencies with a
conflict-directed simulated annealer.
"""

from __future__ import annotations

import argparse
from concurrent.futures import ProcessPoolExecutor, as_completed
import itertools
import math
import random
import re
import time
from dataclasses import dataclass
from pathlib import Path


BOARD = 16
CELLS = BOARD * BOARD
BORDER = 23
TARGET_SCORE = 2 * BOARD * (BOARD - 1)
UP, RIGHT, DOWN, LEFT = range(4)


@dataclass(frozen=True, slots=True)
class Piece:
    pid: int
    edges: tuple[int, int, int, int]


@dataclass(frozen=True, slots=True)
class SwapPlan:
    delta: int
    c1: int
    c2: int
    p1_new: int
    r1_new: int
    p2_new: int
    r2_new: int


@dataclass(frozen=True, slots=True)
class RotatePlan:
    delta: int
    cell: int
    rot: int


@dataclass(frozen=True, slots=True)
class BlockPlan:
    delta: int
    cells: tuple[int, int, int, int]
    pids: tuple[int, int, int, int]
    rots: tuple[int, int, int, int]


@dataclass(frozen=True, slots=True)
class PatchPlan:
    delta: int
    cells: tuple[int, ...]
    pids: tuple[int, ...]
    rots: tuple[int, ...]


def parse_pieces(path: Path) -> list[Piece]:
    pieces: list[Piece] = []
    with path.open("r", encoding="utf-8", errors="ignore") as handle:
        next(handle)
        for line in handle:
            fields = line.split()
            if not fields:
                continue
            pid, up, right, down, left = map(int, fields[:5])
            pieces.append(Piece(pid, (up, right, down, left)))
    if len(pieces) != CELLS:
        raise ValueError(f"expected {CELLS} pieces in {path}, found {len(pieces)}")
    return pieces


def edge_count(edges: tuple[int, int, int, int]) -> int:
    return sum(edge == BORDER for edge in edges)


def rotated(edges: tuple[int, int, int, int], rot: int) -> tuple[int, int, int, int]:
    out = edges
    for _ in range(rot):
        out = (out[LEFT], out[UP], out[RIGHT], out[DOWN])
    return out


def cell_kind(cell: int) -> int:
    row, col = divmod(cell, BOARD)
    return int(row in (0, BOARD - 1)) + int(col in (0, BOARD - 1))


def valid_edges_for_cell(edges: tuple[int, int, int, int], cell: int) -> bool:
    row, col = divmod(cell, BOARD)
    return (
        (edges[UP] == BORDER) == (row == 0)
        and (edges[RIGHT] == BORDER) == (col == BOARD - 1)
        and (edges[DOWN] == BORDER) == (row == BOARD - 1)
        and (edges[LEFT] == BORDER) == (col == 0)
    )


def parse_hint(value: str) -> tuple[int, int, int] | None:
    if value.lower() in {"none", "off", "false", "0"}:
        return None
    fields = value.split(",")
    if len(fields) != 3:
        raise argparse.ArgumentTypeError("hint must be row,col,piece_id or 'none'")
    row, col, pid = map(int, fields)
    if not (1 <= row <= BOARD and 1 <= col <= BOARD):
        raise argparse.ArgumentTypeError("hint row and col are 1-based and must be in 1..16")
    if not (1 <= pid <= CELLS):
        raise argparse.ArgumentTypeError("hint piece id must be in 1..256")
    return row, col, pid


def parse_grid(path: Path) -> tuple[list[int], list[int]]:
    text = path.read_text(encoding="utf-8", errors="ignore")
    pairs = [(int(pid), int(rot)) for pid, rot in re.findall(r"\(\s*(\d+)\s+(\d)\)", text)]
    if len(pairs) != CELLS:
        raise ValueError(f"expected {CELLS} '(piece rot)' entries in {path}, found {len(pairs)}")
    pids = [pid for pid, _ in pairs]
    rots = [rot for _, rot in pairs]
    return pids, rots


def write_grid_file(path: Path, pids: list[int], rots: list[int]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as handle:
        for row in range(BOARD):
            start = row * BOARD
            handle.write(
                "".join(
                    f"({pids[cell]:3d} {rots[cell]:1d})"
                    for cell in range(start, start + BOARD)
                )
            )
            handle.write("\n")


def hungarian_max(weights: list[list[int]]) -> list[int]:
    """Return the max-weight assignment as row -> column in O(n^3)."""
    n = len(weights)
    if n == 0:
        return []
    m = len(weights[0])
    if n > m:
        raise ValueError("hungarian_max requires rows <= columns")

    u = [0] * (n + 1)
    v = [0] * (m + 1)
    p = [0] * (m + 1)
    way = [0] * (m + 1)

    for i in range(1, n + 1):
        p[0] = i
        j0 = 0
        minv = [10**12] * (m + 1)
        used = [False] * (m + 1)
        while True:
            used[j0] = True
            i0 = p[j0]
            delta = 10**12
            j1 = 0
            for j in range(1, m + 1):
                if used[j]:
                    continue
                cur = -weights[i0 - 1][j - 1] - u[i0] - v[j]
                if cur < minv[j]:
                    minv[j] = cur
                    way[j] = j0
                if minv[j] < delta:
                    delta = minv[j]
                    j1 = j
            for j in range(m + 1):
                if used[j]:
                    u[p[j]] += delta
                    v[j] -= delta
                else:
                    minv[j] -= delta
            j0 = j1
            if p[j0] == 0:
                break
        while True:
            j1 = way[j0]
            p[j0] = p[j1]
            j0 = j1
            if j0 == 0:
                break

    assignment = [-1] * n
    for j in range(1, m + 1):
        if p[j] != 0:
            assignment[p[j] - 1] = j - 1
    if any(col < 0 for col in assignment):
        raise RuntimeError("incomplete Hungarian assignment")
    return assignment


class Annealer:
    def __init__(
        self,
        pieces: list[Piece],
        *,
        seed: int,
        hint: tuple[int, int, int] | None,
        save_dir: Path,
        sample_size: int,
        report_every: float,
    ) -> None:
        self.rng = random.Random(seed)
        self.seed = seed
        self.save_dir = save_dir
        self.sample_size = sample_size
        self.report_every = report_every
        self.started = time.perf_counter()
        self.last_report = self.started

        self.pieces = [Piece(0, (0, 0, 0, 0))] + sorted(pieces, key=lambda p: p.pid)
        self.piece_kind = [0] * (CELLS + 1)
        self.rot_edges = [[(0, 0, 0, 0) for _ in range(4)] for _ in range(CELLS + 1)]
        for piece in pieces:
            self.piece_kind[piece.pid] = edge_count(piece.edges)
            for rot in range(4):
                self.rot_edges[piece.pid][rot] = rotated(piece.edges, rot)

        self.cell_kind = [cell_kind(cell) for cell in range(CELLS)]
        self.cells_by_kind = {
            kind: [cell for cell in range(CELLS) if self.cell_kind[cell] == kind]
            for kind in (0, 1, 2)
        }
        self.pieces_by_kind = {
            kind: [piece.pid for piece in pieces if self.piece_kind[piece.pid] == kind]
            for kind in (0, 1, 2)
        }

        self.legal_rots = [
            [() for _ in range(CELLS + 1)]
            for _ in range(CELLS)
        ]
        for cell in range(CELLS):
            for piece in pieces:
                self.legal_rots[cell][piece.pid] = tuple(
                    rot for rot in range(4)
                    if valid_edges_for_cell(self.rot_edges[piece.pid][rot], cell)
                )

        self.edges: list[tuple[int, int, int, int]] = []
        self.cell_edges: list[list[int]] = [[] for _ in range(CELLS)]
        self._build_adjacencies()
        self.edge_marks = [0] * len(self.edges)
        self.mark = 0

        self.fixed_cell: int | None = None
        self.fixed_pid: int | None = None
        self.fixed_rot: int | None = None
        if hint is not None:
            row, col, pid = hint
            cell = (row - 1) * BOARD + (col - 1)
            rots = self.legal_rots[cell][pid]
            if not rots:
                raise ValueError(f"hint piece {pid} cannot legally be placed at row {row}, col {col}")
            self.fixed_cell = cell
            self.fixed_pid = pid
            self.fixed_rot = rots[0]

        self.movable_cells_by_kind = {
            kind: [
                cell for cell in self.cells_by_kind[kind]
                if cell != self.fixed_cell
            ]
            for kind in (0, 1, 2)
        }

        self.pid = [0] * CELLS
        self.rot = [0] * CELLS
        self.score = 0
        self.best_score = -1
        self.best_pid = [0] * CELLS
        self.best_rot = [0] * CELLS
        self.nodes = 0
        self.restarts = 0

    def _build_adjacencies(self) -> None:
        for row in range(BOARD):
            for col in range(BOARD):
                cell = row * BOARD + col
                if col + 1 < BOARD:
                    self._add_edge(cell, cell + 1, RIGHT, LEFT)
                if row + 1 < BOARD:
                    self._add_edge(cell, cell + BOARD, DOWN, UP)

    def _add_edge(self, c1: int, c2: int, s1: int, s2: int) -> None:
        edge_id = len(self.edges)
        self.edges.append((c1, c2, s1, s2))
        self.cell_edges[c1].append(edge_id)
        self.cell_edges[c2].append(edge_id)

    def randomize_board(self) -> None:
        self.pid = [0] * CELLS
        self.rot = [0] * CELLS

        for kind in (0, 1, 2):
            cells = self.cells_by_kind[kind].copy()
            pids = self.pieces_by_kind[kind].copy()

            if self.fixed_cell is not None and self.cell_kind[self.fixed_cell] == kind:
                cells.remove(self.fixed_cell)
                pids.remove(self.fixed_pid)
                self.pid[self.fixed_cell] = self.fixed_pid
                self.rot[self.fixed_cell] = self.fixed_rot

            self.rng.shuffle(cells)
            self.rng.shuffle(pids)
            for cell, pid in zip(cells, pids):
                rots = self.legal_rots[cell][pid]
                if not rots:
                    raise RuntimeError(f"no legal orientation for piece {pid} at cell {cell}")
                self.pid[cell] = pid
                self.rot[cell] = self.rng.choice(rots)

        self.score = self.full_score()
        self.restarts += 1

    def load_board(self, pids: list[int], rots: list[int]) -> None:
        if len(pids) != CELLS or len(rots) != CELLS:
            raise ValueError("loaded board must have 256 pieces and 256 rotations")
        if sorted(pids) != list(range(1, CELLS + 1)):
            raise ValueError("loaded board must contain each piece exactly once")
        for cell, (pid, rot) in enumerate(zip(pids, rots)):
            if rot not in self.legal_rots[cell][pid]:
                raise ValueError(f"piece {pid} rotation {rot} is illegal at cell {cell}")
        if self.fixed_cell is not None:
            if pids[self.fixed_cell] != self.fixed_pid or rots[self.fixed_cell] != self.fixed_rot:
                raise ValueError("loaded board violates the fixed hint")
        self.pid = pids.copy()
        self.rot = rots.copy()
        self.score = self.full_score()
        self.restarts += 1

    def edge_score(self, edge_id: int) -> int:
        c1, c2, s1, s2 = self.edges[edge_id]
        e1 = self.rot_edges[self.pid[c1]][self.rot[c1]][s1]
        e2 = self.rot_edges[self.pid[c2]][self.rot[c2]][s2]
        return int(e1 == e2)

    def full_score(self) -> int:
        return sum(self.edge_score(edge_id) for edge_id in range(len(self.edges)))

    def orientation_score_against_current(self, cell: int, pid: int, rot: int) -> int:
        edges = self.rot_edges[pid][rot]
        total = 0
        for edge_id in self.cell_edges[cell]:
            c1, c2, s1, s2 = self.edges[edge_id]
            if c1 == cell:
                neighbor, side, neighbor_side = c2, s1, s2
            else:
                neighbor, side, neighbor_side = c1, s2, s1
            neighbor_edges = self.rot_edges[self.pid[neighbor]][self.rot[neighbor]]
            total += edges[side] == neighbor_edges[neighbor_side]
        return total

    def best_orientation_against_current(self, cell: int, pid: int) -> tuple[int, int]:
        if cell == self.fixed_cell:
            if pid != self.fixed_pid:
                return -10**6, 0
            rots = (self.fixed_rot,)
        else:
            rots = self.legal_rots[cell][pid]
        if not rots:
            return -10**6, 0

        best_score = -1
        best_rot = rots[0]
        for rot in rots:
            score = self.orientation_score_against_current(cell, pid, rot)
            if score > best_score:
                best_score = score
                best_rot = rot
        return best_score, best_rot

    def assignment_relaxation_pass(
        self,
        *,
        inertia: int,
        jitter: int,
        max_drop: int,
    ) -> int:
        old_pid = self.pid.copy()
        old_rot = self.rot.copy()
        old_score = self.score

        for kind in (0, 1, 2):
            cells = [
                cell for cell in self.cells_by_kind[kind]
                if cell != self.fixed_cell
            ]
            pids = [
                pid for pid in self.pieces_by_kind[kind]
                if pid != self.fixed_pid
            ]
            if not cells:
                continue
            if len(cells) != len(pids):
                raise RuntimeError("cell/piece class size mismatch")

            weights: list[list[int]] = []
            rot_choice: list[list[int]] = []
            for cell in cells:
                row_weights: list[int] = []
                row_rots: list[int] = []
                for pid in pids:
                    score, rot = self.best_orientation_against_current(cell, pid)
                    weight = 100 * score
                    if self.pid[cell] == pid:
                        weight += inertia
                    if jitter > 0:
                        weight += self.rng.randrange(jitter + 1)
                    row_weights.append(weight)
                    row_rots.append(rot)
                weights.append(row_weights)
                rot_choice.append(row_rots)

            assignment = hungarian_max(weights)
            for row, col in enumerate(assignment):
                cell = cells[row]
                self.pid[cell] = pids[col]
                self.rot[cell] = rot_choice[row][col]

        self.score = self.full_score()
        delta = self.score - old_score
        if delta < -max_drop:
            self.pid = old_pid
            self.rot = old_rot
            self.score = old_score
            return 0

        self.remember_best()
        return delta

    def affected_score(self, cells: tuple[int, ...]) -> int:
        self.mark += 1
        total = 0
        for cell in cells:
            for edge_id in self.cell_edges[cell]:
                if self.edge_marks[edge_id] == self.mark:
                    continue
                self.edge_marks[edge_id] = self.mark
                total += self.edge_score(edge_id)
        return total

    def bad_edges(self) -> list[int]:
        return [edge_id for edge_id in range(len(self.edges)) if self.edge_score(edge_id) == 0]

    def bad_cells(self) -> list[int]:
        cells: set[int] = set()
        for edge_id in self.bad_edges():
            c1, c2, _, _ = self.edges[edge_id]
            if c1 != self.fixed_cell:
                cells.add(c1)
            if c2 != self.fixed_cell:
                cells.add(c2)
        out = list(cells)
        self.rng.shuffle(out)
        return out

    def conflicted_cell(self) -> int:
        edge_id = self.rng.choice(self.bad_edges())
        c1, c2, _, _ = self.edges[edge_id]
        if c1 == self.fixed_cell:
            return c2
        if c2 == self.fixed_cell:
            return c1
        return c1 if self.rng.random() < 0.5 else c2

    def random_movable_cell(self, kind: int | None = None) -> int:
        if kind is None:
            kind = self.rng.choices((0, 1, 2), weights=(196, 56, 4), k=1)[0]
        return self.rng.choice(self.movable_cells_by_kind[kind])

    def best_rotate_plan(self, cell: int) -> RotatePlan | None:
        if cell == self.fixed_cell:
            return None
        pid = self.pid[cell]
        rots = self.legal_rots[cell][pid]
        if len(rots) <= 1:
            return None

        old_rot = self.rot[cell]
        before = self.affected_score((cell,))
        best_delta = -999
        best_rot = old_rot
        for new_rot in rots:
            if new_rot == old_rot:
                continue
            self.rot[cell] = new_rot
            delta = self.affected_score((cell,)) - before
            if delta > best_delta:
                best_delta = delta
                best_rot = new_rot
        self.rot[cell] = old_rot
        return RotatePlan(best_delta, cell, best_rot)

    def best_swap_plan(self, c1: int, c2: int) -> SwapPlan | None:
        if c1 == c2 or c1 == self.fixed_cell or c2 == self.fixed_cell:
            return None

        p1, p2 = self.pid[c1], self.pid[c2]
        r1_old, r2_old = self.rot[c1], self.rot[c2]
        p2_rots_at_c1 = self.legal_rots[c1][p2]
        p1_rots_at_c2 = self.legal_rots[c2][p1]
        if not p2_rots_at_c1 or not p1_rots_at_c2:
            return None

        before = self.affected_score((c1, c2))
        best_delta = -999
        best_r1 = r1_old
        best_r2 = r2_old

        self.pid[c1] = p2
        self.pid[c2] = p1
        for r1_new in p2_rots_at_c1:
            self.rot[c1] = r1_new
            for r2_new in p1_rots_at_c2:
                self.rot[c2] = r2_new
                delta = self.affected_score((c1, c2)) - before
                if delta > best_delta:
                    best_delta = delta
                    best_r1 = r1_new
                    best_r2 = r2_new

        self.pid[c1] = p1
        self.pid[c2] = p2
        self.rot[c1] = r1_old
        self.rot[c2] = r2_old

        return SwapPlan(best_delta, c1, c2, p2, best_r1, p1, best_r2)

    def sampled_swap_plan(self, c1: int) -> SwapPlan | None:
        kind = self.cell_kind[c1]
        cells = self.movable_cells_by_kind[kind]
        best: SwapPlan | None = None

        for _ in range(self.sample_size):
            c2 = self.rng.choice(cells)
            plan = self.best_swap_plan(c1, c2)
            if plan is not None and (best is None or plan.delta > best.delta):
                best = plan
                if best.delta >= 4:
                    break
        return best

    def exhaustive_swap_plan(self, c1: int) -> SwapPlan | None:
        kind = self.cell_kind[c1]
        best: SwapPlan | None = None
        for c2 in self.movable_cells_by_kind[kind]:
            plan = self.best_swap_plan(c1, c2)
            if plan is not None and (best is None or plan.delta > best.delta):
                best = plan
        return best

    def best_exhaustive_conflict_swap(self, cells_to_try: int) -> SwapPlan | None:
        best: SwapPlan | None = None
        for cell in self.bad_cells()[:cells_to_try]:
            plan = self.exhaustive_swap_plan(cell)
            if plan is not None and (best is None or plan.delta > best.delta):
                best = plan
                if best.delta >= 4:
                    break
        return best

    def top_swap_plans(self, c1: int, limit: int, max_drop: int) -> list[SwapPlan]:
        kind = self.cell_kind[c1]
        plans: list[SwapPlan] = []
        for c2 in self.movable_cells_by_kind[kind]:
            plan = self.best_swap_plan(c1, c2)
            if plan is not None and plan.delta >= -max_drop:
                plans.append(plan)
        plans.sort(key=lambda plan: plan.delta, reverse=True)
        return plans[:limit]

    def best_double_swap_plan(
        self,
        *,
        cells_to_try: int,
        first_limit: int,
        first_max_drop: int,
    ) -> PatchPlan | None:
        base_pid = self.pid.copy()
        base_rot = self.rot.copy()
        base_score = self.score
        best_delta = -10**9
        best_cells: tuple[int, ...] = ()
        best_pids: tuple[int, ...] = ()
        best_rots: tuple[int, ...] = ()

        for cell in self.bad_cells()[:cells_to_try]:
            for first in self.top_swap_plans(cell, first_limit, first_max_drop):
                self.pid = base_pid.copy()
                self.rot = base_rot.copy()
                self.score = base_score
                self.apply_swap(first)

                second = self.best_exhaustive_conflict_swap(cells_to_try)
                if second is None:
                    continue
                self.apply_swap(second)

                changed = tuple(dict.fromkeys((first.c1, first.c2, second.c1, second.c2)))
                final_pids = tuple(self.pid[changed_cell] for changed_cell in changed)
                final_rots = tuple(self.rot[changed_cell] for changed_cell in changed)

                self.pid = base_pid.copy()
                self.rot = base_rot.copy()
                self.score = base_score
                before = self.affected_score(changed)
                for changed_cell, pid, rot in zip(changed, final_pids, final_rots):
                    self.pid[changed_cell] = pid
                    self.rot[changed_cell] = rot
                delta = self.affected_score(changed) - before

                if delta > best_delta:
                    best_delta = delta
                    best_cells = changed
                    best_pids = final_pids
                    best_rots = final_rots

        self.pid = base_pid
        self.rot = base_rot
        self.score = base_score

        if not best_cells:
            return None
        return PatchPlan(best_delta, best_cells, best_pids, best_rots)

    def best_swap_chain_plan(
        self,
        *,
        depth: int,
        beam_width: int,
        cells_to_try: int,
        top_per_cell: int,
        single_drop: int,
        total_drop: int,
    ) -> PatchPlan | None:
        base_pid = self.pid.copy()
        base_rot = self.rot.copy()
        base_score = self.full_score()
        states: list[tuple[int, list[int], list[int]]] = [(0, base_pid.copy(), base_rot.copy())]
        best_state: tuple[int, list[int], list[int]] | None = None

        for _ in range(max(1, depth)):
            candidates: list[tuple[int, list[int], list[int]]] = []
            for _, state_pid, state_rot in states:
                self.pid = state_pid.copy()
                self.rot = state_rot.copy()
                self.score = self.full_score()
                bad_cells = self.bad_cells()[:cells_to_try]
                for cell in bad_cells:
                    plans = self.top_swap_plans(cell, top_per_cell, single_drop)
                    for plan in plans:
                        self.pid = state_pid.copy()
                        self.rot = state_rot.copy()
                        self.score = self.full_score()
                        self.apply_swap(plan)
                        actual_score = self.full_score()
                        delta = actual_score - base_score
                        if delta < -total_drop:
                            continue
                        if self.pid == base_pid and self.rot == base_rot:
                            continue
                        candidates.append((delta, self.pid.copy(), self.rot.copy()))

            if not candidates:
                break

            candidates.sort(key=lambda item: item[0], reverse=True)
            states = candidates[:beam_width]
            if best_state is None or states[0][0] > best_state[0]:
                best_state = states[0]

        self.pid = base_pid
        self.rot = base_rot
        self.score = base_score

        if best_state is None:
            return None

        delta, final_pid, final_rot = best_state
        changed = tuple(
            cell for cell in range(CELLS)
            if final_pid[cell] != base_pid[cell] or final_rot[cell] != base_rot[cell]
        )
        if not changed:
            return None
        return PatchPlan(
            delta,
            changed,
            tuple(final_pid[cell] for cell in changed),
            tuple(final_rot[cell] for cell in changed),
        )

    def random_swap_plan(self) -> SwapPlan | None:
        kind = self.rng.choices((0, 1, 2), weights=(196, 56, 4), k=1)[0]
        cells = self.movable_cells_by_kind[kind]
        if len(cells) < 2:
            return None
        c1, c2 = self.rng.sample(cells, 2)
        return self.best_swap_plan(c1, c2)

    def block_near_conflict(self) -> tuple[int, int, int, int]:
        cell = self.conflicted_cell()
        row, col = divmod(cell, BOARD)
        top = min(max(row + self.rng.choice((-1, 0)), 0), BOARD - 2)
        left = min(max(col + self.rng.choice((-1, 0)), 0), BOARD - 2)
        c0 = top * BOARD + left
        return c0, c0 + 1, c0 + BOARD, c0 + BOARD + 1

    def patch_near_conflict(self, size: int) -> tuple[int, ...]:
        edge_id = self.rng.choice(self.bad_edges())
        c1, c2, _, _ = self.edges[edge_id]
        center = c1 if self.rng.random() < 0.5 else c2
        row, col = divmod(center, BOARD)
        top = min(max(row - self.rng.randrange(size), 0), BOARD - size)
        left = min(max(col - self.rng.randrange(size), 0), BOARD - size)
        return tuple((top + dr) * BOARD + left + dc for dr in range(size) for dc in range(size))

    def line_near_conflict(self, length: int) -> tuple[int, ...]:
        edge_id = self.rng.choice(self.bad_edges())
        c1, c2, _, _ = self.edges[edge_id]
        r1, c1_col = divmod(c1, BOARD)
        r2, c2_col = divmod(c2, BOARD)
        length = min(max(2, length), BOARD)

        if r1 == r2:
            row = r1
            center = (c1_col + c2_col) // 2
            start = min(max(center - self.rng.randrange(length), 0), BOARD - length)
            return tuple(row * BOARD + start + offset for offset in range(length))

        col = c1_col
        center = (r1 + r2) // 2
        start = min(max(center - self.rng.randrange(length), 0), BOARD - length)
        return tuple((start + offset) * BOARD + col for offset in range(length))

    def pool_near_conflict(self, line_length: int, pool_size: int) -> tuple[int, ...]:
        cells: list[int] = list(self.line_near_conflict(line_length))
        seen = set(cells)

        for cell in self.bad_cells():
            if len(cells) >= pool_size:
                break
            if cell == self.fixed_cell or cell in seen:
                continue
            seen.add(cell)
            cells.append(cell)

        while len(cells) < pool_size:
            cell = self.random_movable_cell()
            if cell in seen:
                continue
            seen.add(cell)
            cells.append(cell)

        return tuple(cells)

    def adjacent_cells(self, cell: int) -> list[int]:
        row, col = divmod(cell, BOARD)
        out: list[int] = []
        if row > 0:
            out.append(cell - BOARD)
        if col < BOARD - 1:
            out.append(cell + 1)
        if row < BOARD - 1:
            out.append(cell + BOARD)
        if col > 0:
            out.append(cell - 1)
        return out

    def targeted_pool_near_conflict(self, core_size: int, pool_size: int) -> tuple[int, ...]:
        core: list[int] = []
        seen_core: set[int] = set()
        bad_edges = self.bad_edges()
        self.rng.shuffle(bad_edges)

        for edge_id in bad_edges:
            c1, c2, _, _ = self.edges[edge_id]
            for cell in (c1, c2):
                if cell not in seen_core:
                    seen_core.add(cell)
                    core.append(cell)
                    if len(core) >= core_size:
                        break
            if len(core) >= core_size:
                break

        for cell in list(core):
            if len(core) >= core_size:
                break
            for neighbor in self.adjacent_cells(cell):
                if neighbor == self.fixed_cell or neighbor in seen_core:
                    continue
                seen_core.add(neighbor)
                core.append(neighbor)
                if len(core) >= core_size:
                    break

        pool = list(core)
        seen = set(pool)
        core_set = set(core)
        pid_cell = [0] * (CELLS + 1)
        for cell, pid in enumerate(self.pid):
            pid_cell[pid] = cell

        donors: list[tuple[int, int]] = []
        for core_cell in core:
            requirements: list[tuple[int, int]] = []
            for edge_id in self.cell_edges[core_cell]:
                c1, c2, s1, s2 = self.edges[edge_id]
                if c1 == core_cell:
                    neighbor, side, neighbor_side = c2, s1, s2
                else:
                    neighbor, side, neighbor_side = c1, s2, s1
                if neighbor in core_set:
                    continue
                neighbor_edges = self.rot_edges[self.pid[neighbor]][self.rot[neighbor]]
                requirements.append((side, neighbor_edges[neighbor_side]))

            if not requirements:
                continue

            for donor_cell in self.cells_by_kind[self.cell_kind[core_cell]]:
                if donor_cell == self.fixed_cell or donor_cell in seen:
                    continue
                pid = self.pid[donor_cell]
                best = 0
                for rot in self.legal_rots[core_cell][pid]:
                    edges = self.rot_edges[pid][rot]
                    matches = sum(edges[side] == color for side, color in requirements)
                    best = max(best, matches)
                if best > 0:
                    donors.append((best, donor_cell))

        self.rng.shuffle(donors)
        donors.sort(reverse=True)
        for _, donor_cell in donors:
            if len(pool) >= pool_size:
                break
            if donor_cell in seen:
                continue
            seen.add(donor_cell)
            pool.append(donor_cell)

        for cell in self.bad_cells():
            if len(pool) >= pool_size:
                break
            if cell == self.fixed_cell or cell in seen:
                continue
            seen.add(cell)
            pool.append(cell)

        while len(pool) < pool_size:
            cell = self.random_movable_cell()
            if cell in seen:
                continue
            seen.add(cell)
            pool.append(cell)

        return tuple(pool)

    def best_block_plan(self, cells: tuple[int, int, int, int]) -> BlockPlan | None:
        old_pids = tuple(self.pid[cell] for cell in cells)
        old_rots = tuple(self.rot[cell] for cell in cells)
        before = self.affected_score(cells)
        best_delta = -999
        best_pids = old_pids
        best_rots = old_rots

        for perm in set(itertools.permutations(old_pids)):
            if self.fixed_cell is not None:
                try:
                    fixed_index = cells.index(self.fixed_cell)
                except ValueError:
                    fixed_index = -1
                if fixed_index >= 0 and perm[fixed_index] != self.fixed_pid:
                    continue

            rot_choices = [
                ((self.fixed_rot,) if cell == self.fixed_cell else self.legal_rots[cell][pid])
                for cell, pid in zip(cells, perm)
            ]
            if any(not choices for choices in rot_choices):
                continue

            for rots in itertools.product(*rot_choices):
                for cell, pid, rot in zip(cells, perm, rots):
                    self.pid[cell] = pid
                    self.rot[cell] = rot
                delta = self.affected_score(cells) - before
                if delta > best_delta:
                    best_delta = delta
                    best_pids = perm
                    best_rots = rots

        for cell, pid, rot in zip(cells, old_pids, old_rots):
            self.pid[cell] = pid
            self.rot[cell] = rot

        return BlockPlan(best_delta, cells, best_pids, best_rots)

    def patch_increment(
        self,
        cell: int,
        pid: int,
        rot: int,
        patch_index: dict[int, int],
        assigned_pids: tuple[int, ...],
        assigned_rots: tuple[int, ...],
    ) -> int:
        total = 0
        cell_edges = self.rot_edges[pid][rot]
        for edge_id in self.cell_edges[cell]:
            c1, c2, s1, s2 = self.edges[edge_id]
            if c1 == cell:
                neighbor, side, neighbor_side = c2, s1, s2
            else:
                neighbor, side, neighbor_side = c1, s2, s1

            neighbor_index = patch_index.get(neighbor)
            if neighbor_index is None:
                neighbor_edges = self.rot_edges[self.pid[neighbor]][self.rot[neighbor]]
                total += cell_edges[side] == neighbor_edges[neighbor_side]
            elif assigned_pids[neighbor_index] != 0:
                neighbor_edges = self.rot_edges[assigned_pids[neighbor_index]][assigned_rots[neighbor_index]]
                total += cell_edges[side] == neighbor_edges[neighbor_side]
        return total

    def patch_bound(self, cells: tuple[int, ...], assigned_mask: int) -> int:
        patch_set = set(cells)
        assigned_cells = {
            cell for index, cell in enumerate(cells)
            if assigned_mask & (1 << index)
        }
        open_edges = 0
        seen: set[int] = set()
        for cell in cells:
            for edge_id in self.cell_edges[cell]:
                if edge_id in seen:
                    continue
                seen.add(edge_id)
                c1, c2, _, _ = self.edges[edge_id]
                if c1 not in patch_set or c2 not in patch_set:
                    if cell not in assigned_cells:
                        open_edges += 1
                elif c1 not in assigned_cells or c2 not in assigned_cells:
                    open_edges += 1
        return open_edges

    def best_patch_plan(self, cells: tuple[int, ...], beam_width: int) -> PatchPlan | None:
        old_pids = tuple(self.pid[cell] for cell in cells)
        old_rots = tuple(self.rot[cell] for cell in cells)
        before = self.affected_score(cells)
        patch_index = {cell: index for index, cell in enumerate(cells)}
        conflict_pressure = {
            cell: sum(1 - self.edge_score(edge_id) for edge_id in self.cell_edges[cell])
            for cell in cells
        }

        legal: list[dict[int, tuple[int, ...]]] = []
        for cell in cells:
            by_pid: dict[int, tuple[int, ...]] = {}
            for pid in old_pids:
                if self.fixed_pid is not None and pid == self.fixed_pid and cell != self.fixed_cell:
                    continue
                if cell == self.fixed_cell:
                    if pid == self.fixed_pid:
                        by_pid[pid] = (self.fixed_rot,)
                    continue
                rots = self.legal_rots[cell][pid]
                if rots:
                    by_pid[pid] = rots
            legal.append(by_pid)

        state = (0, 0, old_pids, (0,) * len(cells), (0,) * len(cells))
        states = [state]
        bound_cache: dict[int, int] = {}

        def bound(mask: int) -> int:
            value = bound_cache.get(mask)
            if value is None:
                value = self.patch_bound(cells, mask)
                bound_cache[mask] = value
            return value

        for _ in cells:
            next_states: list[tuple[int, int, tuple[int, ...], tuple[int, ...], tuple[int, ...]]] = []
            for score, mask, remaining, assigned_pids, assigned_rots in states:
                best_index = -1
                best_count = 10**9
                best_pressure = -1
                for index, cell in enumerate(cells):
                    if mask & (1 << index):
                        continue
                    count = sum(len(legal[index].get(pid, ())) for pid in remaining)
                    pressure = conflict_pressure[cell]
                    if count < best_count or (count == best_count and pressure > best_pressure):
                        best_count = count
                        best_pressure = pressure
                        best_index = index

                if best_index < 0 or best_count == 0:
                    continue

                cell = cells[best_index]
                for pid_pos, pid in enumerate(remaining):
                    rots = legal[best_index].get(pid, ())
                    if not rots:
                        continue
                    new_remaining = remaining[:pid_pos] + remaining[pid_pos + 1:]
                    for rot in rots:
                        increment = self.patch_increment(
                            cell,
                            pid,
                            rot,
                            patch_index,
                            assigned_pids,
                            assigned_rots,
                        )
                        new_pids = list(assigned_pids)
                        new_rots = list(assigned_rots)
                        new_pids[best_index] = pid
                        new_rots[best_index] = rot
                        next_states.append(
                            (
                                score + increment,
                                mask | (1 << best_index),
                                new_remaining,
                                tuple(new_pids),
                                tuple(new_rots),
                            )
                        )

            if not next_states:
                return None

            next_states.sort(
                key=lambda item: (
                    item[0] + bound(item[1]),
                    item[0],
                ),
                reverse=True,
            )
            states = next_states[:beam_width]

        best = max(states, key=lambda item: item[0])
        delta = best[0] - before
        return PatchPlan(delta, cells, best[3], best[4])

    def line_external_score(self, cell: int, pid: int, rot: int, segment: set[int]) -> int:
        total = 0
        cell_edges = self.rot_edges[pid][rot]
        for edge_id in self.cell_edges[cell]:
            c1, c2, s1, s2 = self.edges[edge_id]
            if c1 == cell:
                neighbor, side, neighbor_side = c2, s1, s2
            else:
                neighbor, side, neighbor_side = c1, s2, s1
            if neighbor in segment:
                continue
            neighbor_edges = self.rot_edges[self.pid[neighbor]][self.rot[neighbor]]
            total += cell_edges[side] == neighbor_edges[neighbor_side]
        return total

    def best_line_plan(self, cells: tuple[int, ...], beam_width: int) -> PatchPlan | None:
        if len(cells) < 2:
            return None

        step = cells[1] - cells[0]
        if step == 1:
            prev_side, next_side = RIGHT, LEFT
        elif step == BOARD:
            prev_side, next_side = DOWN, UP
        else:
            raise ValueError("line cells must be contiguous in a row or column")

        old_pids = tuple(self.pid[cell] for cell in cells)
        before = self.affected_score(cells)
        segment = set(cells)
        n = len(cells)

        legal: list[list[tuple[int, ...]]] = []
        external: list[list[dict[int, int]]] = []
        for cell in cells:
            pos_legal: list[tuple[int, ...]] = []
            pos_external: list[dict[int, int]] = []
            for pid in old_pids:
                if self.fixed_pid is not None and pid == self.fixed_pid and cell != self.fixed_cell:
                    rots: tuple[int, ...] = ()
                elif cell == self.fixed_cell:
                    rots = (self.fixed_rot,) if pid == self.fixed_pid else ()
                else:
                    rots = self.legal_rots[cell][pid]
                pos_legal.append(rots)
                pos_external.append({
                    rot: self.line_external_score(cell, pid, rot, segment)
                    for rot in rots
                })
            legal.append(pos_legal)
            external.append(pos_external)

        dp: dict[tuple[int, int, int], int] = {}
        parent: dict[tuple[int, int, int], tuple[int, int, int] | None] = {}
        for pid_index, rots in enumerate(legal[0]):
            for rot in rots:
                key = (1 << pid_index, pid_index, rot)
                score = external[0][pid_index][rot]
                if score > dp.get(key, -10**9):
                    dp[key] = score
                    parent[key] = None

        if not dp:
            return None

        for pos in range(1, n):
            next_dp: dict[tuple[int, int, int], int] = {}
            next_parent: dict[tuple[int, int, int], tuple[int, int, int]] = {}
            for key, score in dp.items():
                mask, last_index, last_rot = key
                last_edges = self.rot_edges[old_pids[last_index]][last_rot]
                for pid_index, rots in enumerate(legal[pos]):
                    bit = 1 << pid_index
                    if mask & bit:
                        continue
                    for rot in rots:
                        edges = self.rot_edges[old_pids[pid_index]][rot]
                        gain = external[pos][pid_index][rot]
                        gain += last_edges[prev_side] == edges[next_side]
                        new_key = (mask | bit, pid_index, rot)
                        new_score = score + gain
                        if new_score > next_dp.get(new_key, -10**9):
                            next_dp[new_key] = new_score
                            next_parent[new_key] = key

            if not next_dp:
                return None

            if beam_width > 0 and len(next_dp) > beam_width:
                keep = set(
                    key for key, _ in sorted(
                        next_dp.items(),
                        key=lambda item: item[1],
                        reverse=True,
                    )[:beam_width]
                )
                next_dp = {key: value for key, value in next_dp.items() if key in keep}
                next_parent = {key: value for key, value in next_parent.items() if key in keep}

            parent.update(next_parent)
            dp = next_dp

        best_key, best_score = max(dp.items(), key=lambda item: item[1])
        new_pids = [0] * n
        new_rots = [0] * n
        key: tuple[int, int, int] | None = best_key
        while key is not None:
            mask, pid_index, rot = key
            pos = mask.bit_count() - 1
            new_pids[pos] = old_pids[pid_index]
            new_rots[pos] = rot
            key = parent[key]

        return PatchPlan(best_score - before, cells, tuple(new_pids), tuple(new_rots))

    def best_pool_assignment_plan(
        self,
        cells: tuple[int, ...],
        *,
        inertia: int,
        jitter: int,
    ) -> PatchPlan | None:
        cells = tuple(dict.fromkeys(cells))
        old_pids = tuple(self.pid[cell] for cell in cells)
        old_rots = tuple(self.rot[cell] for cell in cells)
        new_pids = list(old_pids)
        new_rots = list(old_rots)

        for kind in (0, 1, 2):
            indexes = [index for index, cell in enumerate(cells) if self.cell_kind[cell] == kind]
            if not indexes:
                continue

            kind_cells = [cells[index] for index in indexes]
            kind_pids = [old_pids[index] for index in indexes]
            weights: list[list[int]] = []
            rot_choice: list[list[int]] = []

            for cell in kind_cells:
                row_weights: list[int] = []
                row_rots: list[int] = []
                for pid in kind_pids:
                    score, rot = self.best_orientation_against_current(cell, pid)
                    if score < 0:
                        weight = -10**9
                    else:
                        weight = 100 * score
                        if self.pid[cell] == pid:
                            weight += inertia
                        if jitter > 0:
                            weight += self.rng.randrange(jitter + 1)
                    row_weights.append(weight)
                    row_rots.append(rot)
                weights.append(row_weights)
                rot_choice.append(row_rots)

            assignment = hungarian_max(weights)
            for row, col in enumerate(assignment):
                if weights[row][col] <= -10**8:
                    return None
                target_index = indexes[row]
                new_pids[target_index] = kind_pids[col]
                new_rots[target_index] = rot_choice[row][col]

        before = self.affected_score(cells)
        for cell, pid, rot in zip(cells, new_pids, new_rots):
            self.pid[cell] = pid
            self.rot[cell] = rot
        after = self.affected_score(cells)
        for cell, pid, rot in zip(cells, old_pids, old_rots):
            self.pid[cell] = pid
            self.rot[cell] = rot

        return PatchPlan(after - before, cells, tuple(new_pids), tuple(new_rots))

    def random_pool_shuffle_plan(self, cells: tuple[int, ...]) -> PatchPlan | None:
        cells = tuple(dict.fromkeys(cells))
        old_pids = tuple(self.pid[cell] for cell in cells)
        old_rots = tuple(self.rot[cell] for cell in cells)
        new_pids = list(old_pids)
        new_rots = list(old_rots)

        for kind in (0, 1, 2):
            indexes = [
                index for index, cell in enumerate(cells)
                if self.cell_kind[cell] == kind and cell != self.fixed_cell
            ]
            if not indexes:
                continue
            pids = [old_pids[index] for index in indexes]
            self.rng.shuffle(pids)
            for target_index, pid in zip(indexes, pids):
                cell = cells[target_index]
                rots = self.legal_rots[cell][pid]
                if not rots:
                    return None
                new_pids[target_index] = pid
                new_rots[target_index] = self.rng.choice(rots)

        before = self.affected_score(cells)
        for cell, pid, rot in zip(cells, new_pids, new_rots):
            self.pid[cell] = pid
            self.rot[cell] = rot
        after = self.affected_score(cells)
        for cell, pid, rot in zip(cells, old_pids, old_rots):
            self.pid[cell] = pid
            self.rot[cell] = rot

        return PatchPlan(after - before, cells, tuple(new_pids), tuple(new_rots))

    def accept(self, delta: int, temperature: float) -> bool:
        if delta >= 0:
            return True
        return self.rng.random() < math.exp(delta / max(temperature, 1.0e-9))

    def apply_rotate(self, plan: RotatePlan) -> None:
        self.rot[plan.cell] = plan.rot
        self.score += plan.delta

    def apply_swap(self, plan: SwapPlan) -> None:
        self.pid[plan.c1] = plan.p1_new
        self.rot[plan.c1] = plan.r1_new
        self.pid[plan.c2] = plan.p2_new
        self.rot[plan.c2] = plan.r2_new
        self.score += plan.delta

    def apply_block(self, plan: BlockPlan) -> None:
        for cell, pid, rot in zip(plan.cells, plan.pids, plan.rots):
            self.pid[cell] = pid
            self.rot[cell] = rot
        self.score += plan.delta

    def apply_patch_plan(self, plan: PatchPlan) -> None:
        for cell, pid, rot in zip(plan.cells, plan.pids, plan.rots):
            self.pid[cell] = pid
            self.rot[cell] = rot
        self.score += plan.delta

    def remember_best(self) -> None:
        if self.score <= self.best_score:
            return
        self.best_score = self.score
        self.best_pid = self.pid.copy()
        self.best_rot = self.rot.copy()
        self.write_grid(self.save_dir / "best_full.txt")
        elapsed = time.perf_counter() - self.started
        print(
            f"best score={self.best_score:3d}/{TARGET_SCORE} "
            f"restart={self.restarts} nodes={self.nodes} elapsed={elapsed:.1f}s",
            flush=True,
        )

    def report(self) -> None:
        now = time.perf_counter()
        if now - self.last_report < self.report_every:
            return
        self.last_report = now
        print(
            f"search score={self.score:3d} best={self.best_score:3d} "
            f"restart={self.restarts} nodes={self.nodes}",
            flush=True,
        )

    def search_step(
        self,
        *,
        step: int,
        total_steps: int,
        temp_start: float,
        temp_end: float,
        patch_size: int,
        patch_beam: int,
        patch_rate: float,
        line_length: int,
        line_beam: int,
        line_rate: float,
        pool_size: int,
        pool_rate: float,
        pool_inertia: int,
        pool_jitter: int,
        chain_rate: float,
        chain_depth: int,
        chain_beam: int,
        chain_first: int,
        chain_drop: int,
        exhaustive_rate: float,
        exhaustive_cells: int,
    ) -> None:
        frac = step / max(1, total_steps - 1)
        temperature = temp_start * ((temp_end / temp_start) ** frac)
        self.nodes += 1

        move_roll = self.rng.random()
        if move_roll < pool_rate:
            if self.rng.random() < 0.65:
                plan = self.best_patch_plan(
                    self.targeted_pool_near_conflict(
                        max(4, min(line_length, pool_size // 2)),
                        pool_size,
                    ),
                    max(line_beam, patch_beam),
                )
            else:
                plan = self.best_pool_assignment_plan(
                    self.pool_near_conflict(line_length, pool_size),
                    inertia=pool_inertia,
                    jitter=pool_jitter,
                )
            if plan is not None and self.accept(plan.delta, temperature):
                self.apply_patch_plan(plan)
        elif move_roll < pool_rate + chain_rate:
            plan = self.best_swap_chain_plan(
                depth=chain_depth,
                beam_width=chain_beam,
                cells_to_try=exhaustive_cells,
                top_per_cell=chain_first,
                single_drop=chain_drop,
                total_drop=chain_drop * max(1, chain_depth),
            )
            if plan is not None and self.accept(plan.delta, temperature):
                self.apply_patch_plan(plan)
        elif move_roll < pool_rate + chain_rate + line_rate:
            plan = self.best_line_plan(self.line_near_conflict(line_length), line_beam)
            if plan is not None and self.accept(plan.delta, temperature):
                self.apply_patch_plan(plan)
        elif move_roll < pool_rate + chain_rate + line_rate + patch_rate:
            plan = self.best_patch_plan(self.patch_near_conflict(patch_size), patch_beam)
            if plan is not None and self.accept(plan.delta, temperature):
                self.apply_patch_plan(plan)
        elif move_roll < pool_rate + chain_rate + line_rate + patch_rate + exhaustive_rate:
            plan = self.best_exhaustive_conflict_swap(exhaustive_cells)
            if plan is not None and self.accept(plan.delta, temperature):
                self.apply_swap(plan)
        elif move_roll < pool_rate + chain_rate + line_rate + patch_rate + exhaustive_rate + 0.04:
            plan = self.best_block_plan(self.block_near_conflict())
            if plan is not None and self.accept(plan.delta, temperature):
                self.apply_block(plan)
        elif move_roll < pool_rate + chain_rate + line_rate + patch_rate + exhaustive_rate + 0.20:
            plan = self.best_rotate_plan(self.conflicted_cell())
            if plan is not None and self.accept(plan.delta, temperature):
                self.apply_rotate(plan)
        elif move_roll < pool_rate + chain_rate + line_rate + patch_rate + exhaustive_rate + 0.90:
            plan = self.sampled_swap_plan(self.conflicted_cell())
            if plan is not None and self.accept(plan.delta, temperature):
                self.apply_swap(plan)
        else:
            plan = self.random_swap_plan()
            if plan is not None and self.accept(plan.delta, temperature):
                self.apply_swap(plan)

        self.remember_best()
        self.report()

    def start_from(
        self,
        initial_board: tuple[list[int], list[int]] | None,
    ) -> None:
        if initial_board is None:
            self.randomize_board()
        else:
            self.load_board(*initial_board)
        self.remember_best()

    def restore_best(self) -> None:
        if self.best_score < 0:
            return
        self.pid = self.best_pid.copy()
        self.rot = self.best_rot.copy()
        self.score = self.best_score

    def run(
        self,
        *,
        seconds: float,
        restart_steps: int,
        temp_start: float,
        temp_end: float,
        patch_size: int,
        patch_beam: int,
        patch_rate: float,
        line_length: int,
        line_beam: int,
        line_rate: float,
        pool_size: int,
        pool_rate: float,
        pool_inertia: int,
        pool_jitter: int,
        chain_rate: float,
        chain_depth: int,
        chain_beam: int,
        chain_first: int,
        chain_drop: int,
        exhaustive_rate: float,
        exhaustive_cells: int,
        initial_board: tuple[list[int], list[int]] | None = None,
    ) -> None:
        deadline = time.perf_counter() + seconds
        while time.perf_counter() < deadline and self.best_score < TARGET_SCORE:
            self.start_from(initial_board)
            initial_board = None

            for step in range(restart_steps):
                if time.perf_counter() >= deadline or self.best_score == TARGET_SCORE:
                    break

                self.search_step(
                    step=step,
                    total_steps=restart_steps,
                    temp_start=temp_start,
                    temp_end=temp_end,
                    patch_size=patch_size,
                    patch_beam=patch_beam,
                    patch_rate=patch_rate,
                    line_length=line_length,
                    line_beam=line_beam,
                    line_rate=line_rate,
                    pool_size=pool_size,
                    pool_rate=pool_rate,
                    pool_inertia=pool_inertia,
                    pool_jitter=pool_jitter,
                    chain_rate=chain_rate,
                    chain_depth=chain_depth,
                    chain_beam=chain_beam,
                    chain_first=chain_first,
                    chain_drop=chain_drop,
                    exhaustive_rate=exhaustive_rate,
                    exhaustive_cells=exhaustive_cells,
                )

    def run_polynomial(
        self,
        *,
        passes: int,
        temp_start: float,
        temp_end: float,
        patch_size: int,
        patch_beam: int,
        patch_rate: float,
        line_length: int,
        line_beam: int,
        line_rate: float,
        pool_size: int,
        pool_rate: float,
        pool_inertia: int,
        pool_jitter: int,
        chain_rate: float,
        chain_depth: int,
        chain_beam: int,
        chain_first: int,
        chain_drop: int,
        exhaustive_rate: float,
        exhaustive_cells: int,
        initial_board: tuple[list[int], list[int]] | None = None,
    ) -> None:
        total_steps = max(0, passes) * CELLS * CELLS
        self.start_from(initial_board)
        print(
            f"polynomial budget passes={passes} steps={total_steps} "
            f"complexity=O(passes*n^3) for fixed patch/beam",
            flush=True,
        )
        for step in range(total_steps):
            if self.best_score == TARGET_SCORE:
                break
            self.search_step(
                step=step,
                total_steps=total_steps,
                temp_start=temp_start,
                temp_end=temp_end,
                patch_size=patch_size,
                patch_beam=patch_beam,
                patch_rate=patch_rate,
                line_length=line_length,
                line_beam=line_beam,
                line_rate=line_rate,
                pool_size=pool_size,
                pool_rate=pool_rate,
                pool_inertia=pool_inertia,
                pool_jitter=pool_jitter,
                chain_rate=chain_rate,
                chain_depth=chain_depth,
                chain_beam=chain_beam,
                chain_first=chain_first,
                chain_drop=chain_drop,
                exhaustive_rate=exhaustive_rate,
                exhaustive_cells=exhaustive_cells,
            )

    def run_assignment_relaxation(
        self,
        *,
        passes: int,
        polish_steps: int,
        inertia: int,
        jitter: int,
        max_drop: int,
        temp_start: float,
        temp_end: float,
        patch_size: int,
        patch_beam: int,
        patch_rate: float,
        line_length: int,
        line_beam: int,
        line_rate: float,
        pool_size: int,
        pool_rate: float,
        pool_inertia: int,
        pool_jitter: int,
        chain_rate: float,
        chain_depth: int,
        chain_beam: int,
        chain_first: int,
        chain_drop: int,
        exhaustive_rate: float,
        exhaustive_cells: int,
        initial_board: tuple[list[int], list[int]] | None = None,
    ) -> None:
        self.start_from(initial_board)
        print(
            f"assignment relaxation passes={passes} polish_steps={polish_steps} "
            f"complexity=O(passes*n^3 + passes*polish)",
            flush=True,
        )
        total_polish = max(1, passes * max(1, polish_steps))
        for pass_id in range(max(0, passes)):
            delta = self.assignment_relaxation_pass(
                inertia=inertia,
                jitter=jitter,
                max_drop=max_drop,
            )
            print(
                f"relax pass={pass_id + 1} delta={delta:+d} "
                f"score={self.score}/{TARGET_SCORE} best={self.best_score}",
                flush=True,
            )
            for local_step in range(max(0, polish_steps)):
                if self.best_score == TARGET_SCORE:
                    return
                self.search_step(
                    step=pass_id * max(1, polish_steps) + local_step,
                    total_steps=total_polish,
                    temp_start=temp_start,
                    temp_end=temp_end,
                    patch_size=patch_size,
                    patch_beam=patch_beam,
                    patch_rate=patch_rate,
                    line_length=line_length,
                    line_beam=line_beam,
                    line_rate=line_rate,
                    pool_size=pool_size,
                    pool_rate=pool_rate,
                    pool_inertia=pool_inertia,
                    pool_jitter=pool_jitter,
                    chain_rate=chain_rate,
                    chain_depth=chain_depth,
                    chain_beam=chain_beam,
                    chain_first=chain_first,
                    chain_drop=chain_drop,
                    exhaustive_rate=exhaustive_rate,
                    exhaustive_cells=exhaustive_cells,
                )

    def run_basin_hops(
        self,
        *,
        jumps: int,
        shake_size: int,
        shake_drop: int,
        shake_tries: int,
        polish_steps: int,
        temp_start: float,
        temp_end: float,
        patch_size: int,
        patch_beam: int,
        patch_rate: float,
        line_length: int,
        line_beam: int,
        line_rate: float,
        pool_size: int,
        pool_rate: float,
        pool_inertia: int,
        pool_jitter: int,
        chain_rate: float,
        chain_depth: int,
        chain_beam: int,
        chain_first: int,
        chain_drop: int,
        exhaustive_rate: float,
        exhaustive_cells: int,
        initial_board: tuple[list[int], list[int]] | None = None,
    ) -> None:
        self.start_from(initial_board)
        total_steps = max(1, jumps * max(1, polish_steps))
        print(
            f"basin hops jumps={jumps} shake_size={shake_size} "
            f"shake_drop={shake_drop} polish_steps={polish_steps}",
            flush=True,
        )

        for jump in range(max(0, jumps)):
            if self.best_score == TARGET_SCORE:
                return
            self.restore_best()

            best_shake: PatchPlan | None = None
            for _ in range(max(1, shake_tries)):
                cells = self.pool_near_conflict(line_length, shake_size)
                plan = self.random_pool_shuffle_plan(cells)
                if plan is None:
                    continue
                if best_shake is None or plan.delta > best_shake.delta:
                    best_shake = plan
                if plan.delta >= -shake_drop and plan.delta != 0:
                    best_shake = plan
                    break

            if best_shake is None:
                continue
            if best_shake.delta < -shake_drop:
                continue

            self.apply_patch_plan(best_shake)
            print(
                f"jump={jump + 1} shake_delta={best_shake.delta:+d} "
                f"score={self.score} best={self.best_score}",
                flush=True,
            )

            for local_step in range(max(0, polish_steps)):
                if self.best_score == TARGET_SCORE:
                    return
                self.search_step(
                    step=jump * max(1, polish_steps) + local_step,
                    total_steps=total_steps,
                    temp_start=temp_start,
                    temp_end=temp_end,
                    patch_size=patch_size,
                    patch_beam=patch_beam,
                    patch_rate=patch_rate,
                    line_length=line_length,
                    line_beam=line_beam,
                    line_rate=line_rate,
                    pool_size=pool_size,
                    pool_rate=pool_rate,
                    pool_inertia=pool_inertia,
                    pool_jitter=pool_jitter,
                    chain_rate=chain_rate,
                    chain_depth=chain_depth,
                    chain_beam=chain_beam,
                    chain_first=chain_first,
                    chain_drop=chain_drop,
                    exhaustive_rate=exhaustive_rate,
                    exhaustive_cells=exhaustive_cells,
                )

    def validate_best(self) -> None:
        if sorted(self.best_pid) != list(range(1, CELLS + 1)):
            raise AssertionError("best board does not contain each piece exactly once")
        for cell, (pid, rot) in enumerate(zip(self.best_pid, self.best_rot)):
            if rot not in self.legal_rots[cell][pid]:
                raise AssertionError(f"piece {pid} rotation {rot} is illegal at cell {cell}")
        if self.fixed_cell is not None and (
            self.best_pid[self.fixed_cell] != self.fixed_pid
            or self.best_rot[self.fixed_cell] != self.fixed_rot
        ):
            raise AssertionError("best board violates the fixed hint")

    def write_grid(self, path: Path) -> None:
        write_grid_file(path, self.best_pid, self.best_rot)


def run_worker(
    worker_id: int,
    pieces: list[Piece],
    seed: int,
    hint: tuple[int, int, int] | None,
    save_dir: Path,
    sample_size: int,
    report_every: float,
    seconds: float,
    basin_jumps: int,
    shake_size: int,
    shake_drop: int,
    shake_tries: int,
    shake_polish: int,
    relax_passes: int,
    relax_polish: int,
    relax_inertia: int,
    relax_jitter: int,
    relax_max_drop: int,
    poly_passes: int,
    restart_steps: int,
    temp_start: float,
    temp_end: float,
    patch_size: int,
    patch_beam: int,
    patch_rate: float,
    line_length: int,
    line_beam: int,
    line_rate: float,
    pool_size: int,
    pool_rate: float,
    pool_inertia: int,
    pool_jitter: int,
    chain_rate: float,
    chain_depth: int,
    chain_beam: int,
    chain_first: int,
    chain_drop: int,
    exhaustive_rate: float,
    exhaustive_cells: int,
    initial_board: tuple[list[int], list[int]] | None,
) -> tuple[int, list[int], list[int], int]:
    solver = Annealer(
        pieces,
        seed=seed,
        hint=hint,
        save_dir=save_dir / f"worker_{worker_id:02d}",
        sample_size=sample_size,
        report_every=report_every,
    )
    print(f"worker {worker_id} seed={seed}", flush=True)
    if basin_jumps > 0:
        solver.run_basin_hops(
            jumps=basin_jumps,
            shake_size=shake_size,
            shake_drop=shake_drop,
            shake_tries=shake_tries,
            polish_steps=shake_polish,
            temp_start=temp_start,
            temp_end=temp_end,
            patch_size=patch_size,
            patch_beam=patch_beam,
            patch_rate=patch_rate,
            line_length=line_length,
            line_beam=line_beam,
            line_rate=line_rate,
            pool_size=pool_size,
            pool_rate=pool_rate,
            pool_inertia=pool_inertia,
            pool_jitter=pool_jitter,
            chain_rate=chain_rate,
            chain_depth=chain_depth,
            chain_beam=chain_beam,
            chain_first=chain_first,
            chain_drop=chain_drop,
            exhaustive_rate=exhaustive_rate,
            exhaustive_cells=exhaustive_cells,
            initial_board=initial_board,
        )
    elif relax_passes > 0:
        solver.run_assignment_relaxation(
            passes=relax_passes,
            polish_steps=relax_polish,
            inertia=relax_inertia,
            jitter=relax_jitter,
            max_drop=relax_max_drop,
            temp_start=temp_start,
            temp_end=temp_end,
            patch_size=patch_size,
            patch_beam=patch_beam,
            patch_rate=patch_rate,
            line_length=line_length,
            line_beam=line_beam,
            line_rate=line_rate,
            pool_size=pool_size,
            pool_rate=pool_rate,
            pool_inertia=pool_inertia,
            pool_jitter=pool_jitter,
            chain_rate=chain_rate,
            chain_depth=chain_depth,
            chain_beam=chain_beam,
            chain_first=chain_first,
            chain_drop=chain_drop,
            exhaustive_rate=exhaustive_rate,
            exhaustive_cells=exhaustive_cells,
            initial_board=initial_board,
        )
    elif poly_passes > 0:
        solver.run_polynomial(
            passes=poly_passes,
            temp_start=temp_start,
            temp_end=temp_end,
            patch_size=patch_size,
            patch_beam=patch_beam,
            patch_rate=patch_rate,
            line_length=line_length,
            line_beam=line_beam,
            line_rate=line_rate,
            pool_size=pool_size,
            pool_rate=pool_rate,
            pool_inertia=pool_inertia,
            pool_jitter=pool_jitter,
            chain_rate=chain_rate,
            chain_depth=chain_depth,
            chain_beam=chain_beam,
            chain_first=chain_first,
            chain_drop=chain_drop,
            exhaustive_rate=exhaustive_rate,
            exhaustive_cells=exhaustive_cells,
            initial_board=initial_board,
        )
    else:
        solver.run(
            seconds=seconds,
            restart_steps=restart_steps,
            temp_start=temp_start,
            temp_end=temp_end,
            patch_size=patch_size,
            patch_beam=patch_beam,
            patch_rate=patch_rate,
            line_length=line_length,
            line_beam=line_beam,
            line_rate=line_rate,
            pool_size=pool_size,
            pool_rate=pool_rate,
            pool_inertia=pool_inertia,
            pool_jitter=pool_jitter,
            exhaustive_rate=exhaustive_rate,
            exhaustive_cells=exhaustive_cells,
            initial_board=initial_board,
        )
    solver.validate_best()
    return solver.best_score, solver.best_pid, solver.best_rot, worker_id


def main() -> int:
    parser = argparse.ArgumentParser(description="Full-board annealing solver for Eternity II.")
    parser.add_argument("--pieces", type=Path, default=Path("pieces.txt"))
    parser.add_argument("--seconds", type=float, default=60.0, help="wall-clock limit for anytime mode")
    parser.add_argument(
        "--poly-passes",
        type=int,
        default=0,
        help="enable polynomial-budget mode with passes*n^2 moves; disables the wall-clock loop",
    )
    parser.add_argument(
        "--relax-passes",
        type=int,
        default=0,
        help="enable Hungarian assignment-relaxation mode; overrides --poly-passes and --seconds",
    )
    parser.add_argument("--basin-jumps", type=int, default=0)
    parser.add_argument("--shake-size", type=int, default=96)
    parser.add_argument("--shake-drop", type=int, default=32)
    parser.add_argument("--shake-tries", type=int, default=64)
    parser.add_argument("--shake-polish", type=int, default=2048)
    parser.add_argument("--relax-polish", type=int, default=512)
    parser.add_argument("--relax-inertia", type=int, default=15)
    parser.add_argument("--relax-jitter", type=int, default=3)
    parser.add_argument("--relax-max-drop", type=int, default=2)
    parser.add_argument("--seed", type=int, default=None)
    parser.add_argument("--save-dir", type=Path, default=Path("full_solver_grids"))
    parser.add_argument("--start-grid", type=Path, default=None)
    parser.add_argument("--workers", type=int, default=1)
    parser.add_argument("--restart-steps", type=int, default=250_000)
    parser.add_argument("--sample-size", type=int, default=32)
    parser.add_argument("--patch-size", type=int, default=3, choices=(2, 3, 4, 5))
    parser.add_argument("--patch-beam", type=int, default=700)
    parser.add_argument("--patch-rate", type=float, default=0.03)
    parser.add_argument("--line-length", type=int, default=12)
    parser.add_argument("--line-beam", type=int, default=8000)
    parser.add_argument("--line-rate", type=float, default=0.04)
    parser.add_argument("--pool-size", type=int, default=48)
    parser.add_argument("--pool-rate", type=float, default=0.06)
    parser.add_argument("--pool-inertia", type=int, default=12)
    parser.add_argument("--pool-jitter", type=int, default=3)
    parser.add_argument("--chain-rate", type=float, default=0.04)
    parser.add_argument("--chain-depth", type=int, default=3)
    parser.add_argument("--chain-beam", type=int, default=24)
    parser.add_argument("--chain-first", type=int, default=8)
    parser.add_argument("--chain-drop", type=int, default=2)
    parser.add_argument("--exhaustive-rate", type=float, default=0.07)
    parser.add_argument("--exhaustive-cells", type=int, default=4)
    parser.add_argument("--temp-start", type=float, default=2.4)
    parser.add_argument("--temp-end", type=float, default=0.03)
    parser.add_argument("--report-every", type=float, default=10.0)
    parser.add_argument(
        "--hint",
        type=parse_hint,
        default=parse_hint("9,8,139"),
        help="fixed hint as row,col,piece_id using 1-based board coordinates; use 'none' to disable",
    )
    args = parser.parse_args()

    pieces = parse_pieces(args.pieces)
    seed = args.seed if args.seed is not None else random.randrange(2**32)
    if args.start_grid is not None:
        initial_board = parse_grid(args.start_grid)
    elif (args.save_dir / "best_full.txt").exists():
        initial_board = parse_grid(args.save_dir / "best_full.txt")
        print(f"resuming existing grid {args.save_dir / 'best_full.txt'}", flush=True)
    else:
        initial_board = None

    print(
        f"full-board anneal seed={seed} limit={args.seconds:.1f}s "
        f"target={TARGET_SCORE} hint={args.hint} workers={args.workers} "
        f"basin_jumps={args.basin_jumps} relax_passes={args.relax_passes} "
        f"poly_passes={args.poly_passes}",
        flush=True,
    )

    if args.workers <= 1:
        solver = Annealer(
            pieces,
            seed=seed,
            hint=args.hint,
            save_dir=args.save_dir,
            sample_size=args.sample_size,
            report_every=args.report_every,
        )
        if args.basin_jumps > 0:
            solver.run_basin_hops(
                jumps=args.basin_jumps,
                shake_size=args.shake_size,
                shake_drop=args.shake_drop,
                shake_tries=args.shake_tries,
                polish_steps=args.shake_polish,
                temp_start=args.temp_start,
                temp_end=args.temp_end,
                patch_size=args.patch_size,
                patch_beam=args.patch_beam,
                patch_rate=args.patch_rate,
                line_length=args.line_length,
                line_beam=args.line_beam,
                line_rate=args.line_rate,
                pool_size=args.pool_size,
                pool_rate=args.pool_rate,
                pool_inertia=args.pool_inertia,
                pool_jitter=args.pool_jitter,
                chain_rate=args.chain_rate,
                chain_depth=args.chain_depth,
                chain_beam=args.chain_beam,
                chain_first=args.chain_first,
                chain_drop=args.chain_drop,
                exhaustive_rate=args.exhaustive_rate,
                exhaustive_cells=args.exhaustive_cells,
                initial_board=initial_board,
            )
        elif args.relax_passes > 0:
            solver.run_assignment_relaxation(
                passes=args.relax_passes,
                polish_steps=args.relax_polish,
                inertia=args.relax_inertia,
                jitter=args.relax_jitter,
                max_drop=args.relax_max_drop,
                temp_start=args.temp_start,
                temp_end=args.temp_end,
                patch_size=args.patch_size,
                patch_beam=args.patch_beam,
                patch_rate=args.patch_rate,
                line_length=args.line_length,
                line_beam=args.line_beam,
                line_rate=args.line_rate,
                pool_size=args.pool_size,
                pool_rate=args.pool_rate,
                pool_inertia=args.pool_inertia,
                pool_jitter=args.pool_jitter,
                chain_rate=args.chain_rate,
                chain_depth=args.chain_depth,
                chain_beam=args.chain_beam,
                chain_first=args.chain_first,
                chain_drop=args.chain_drop,
                exhaustive_rate=args.exhaustive_rate,
                exhaustive_cells=args.exhaustive_cells,
                initial_board=initial_board,
            )
        elif args.poly_passes > 0:
            solver.run_polynomial(
                passes=args.poly_passes,
                temp_start=args.temp_start,
                temp_end=args.temp_end,
                patch_size=args.patch_size,
                patch_beam=args.patch_beam,
                patch_rate=args.patch_rate,
                line_length=args.line_length,
                line_beam=args.line_beam,
                line_rate=args.line_rate,
                pool_size=args.pool_size,
                pool_rate=args.pool_rate,
                pool_inertia=args.pool_inertia,
                pool_jitter=args.pool_jitter,
                chain_rate=args.chain_rate,
                chain_depth=args.chain_depth,
                chain_beam=args.chain_beam,
                chain_first=args.chain_first,
                chain_drop=args.chain_drop,
                exhaustive_rate=args.exhaustive_rate,
                exhaustive_cells=args.exhaustive_cells,
                initial_board=initial_board,
            )
        else:
            solver.run(
                seconds=args.seconds,
                restart_steps=args.restart_steps,
                temp_start=args.temp_start,
                temp_end=args.temp_end,
                patch_size=args.patch_size,
                patch_beam=args.patch_beam,
                patch_rate=args.patch_rate,
                line_length=args.line_length,
                line_beam=args.line_beam,
                line_rate=args.line_rate,
                pool_size=args.pool_size,
                pool_rate=args.pool_rate,
                pool_inertia=args.pool_inertia,
                pool_jitter=args.pool_jitter,
                chain_rate=args.chain_rate,
                chain_depth=args.chain_depth,
                chain_beam=args.chain_beam,
                chain_first=args.chain_first,
                chain_drop=args.chain_drop,
                exhaustive_rate=args.exhaustive_rate,
                exhaustive_cells=args.exhaustive_cells,
                initial_board=initial_board,
            )
        solver.validate_best()
        best_score = solver.best_score
    else:
        best_score = -1
        with ProcessPoolExecutor(max_workers=args.workers) as pool:
            futures = [
                pool.submit(
                    run_worker,
                    worker_id,
                    pieces,
                    seed + worker_id - 1,
                    args.hint,
                    args.save_dir,
                    args.sample_size,
                    args.report_every,
                    args.seconds,
                    args.basin_jumps,
                    args.shake_size,
                    args.shake_drop,
                    args.shake_tries,
                    args.shake_polish,
                    args.relax_passes,
                    args.relax_polish,
                    args.relax_inertia,
                    args.relax_jitter,
                    args.relax_max_drop,
                    args.poly_passes,
                    args.restart_steps,
                    args.temp_start,
                    args.temp_end,
                    args.patch_size,
                    args.patch_beam,
                    args.patch_rate,
                    args.line_length,
                    args.line_beam,
                    args.line_rate,
                    args.pool_size,
                    args.pool_rate,
                    args.pool_inertia,
                    args.pool_jitter,
                    args.chain_rate,
                    args.chain_depth,
                    args.chain_beam,
                    args.chain_first,
                    args.chain_drop,
                    args.exhaustive_rate,
                    args.exhaustive_cells,
                    initial_board,
                )
                for worker_id in range(1, args.workers + 1)
            ]
            for future in as_completed(futures):
                score, pids, rots, worker_id = future.result()
                print(f"worker {worker_id} best score={score}/{TARGET_SCORE}", flush=True)
                if score > best_score:
                    best_score = score
                    write_grid_file(args.save_dir / "best_full.txt", pids, rots)

    print(f"overall best score={best_score}/{TARGET_SCORE} grid={args.save_dir / 'best_full.txt'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
