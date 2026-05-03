#!/usr/bin/env python3
"""Constraint solver for the Eternity II edge-matching puzzle.

The search is deliberately built as an anytime solver: it keeps improving the
best partial grid it has found, writes that grid to disk, and can be restarted
with different random seeds.  A full Eternity II proof search is still a very
large computation, but this is a much stronger baseline than the original
shuffle-and-greedy Fortran loop.
"""

from __future__ import annotations

import argparse
import random
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


BOARD = 16
BORDER = 23
UNKNOWN = -1


@dataclass(frozen=True, slots=True)
class Piece:
    pid: int
    up: int
    right: int
    down: int
    left: int


@dataclass(frozen=True, slots=True)
class OrientedPiece:
    pid: int
    rot: int
    up: int
    right: int
    down: int
    left: int


class Timeout(Exception):
    pass


def parse_pieces(path: Path) -> list[Piece]:
    pieces: list[Piece] = []
    with path.open("r", encoding="utf-8", errors="ignore") as handle:
        next(handle)
        for line in handle:
            fields = line.split()
            if not fields:
                continue
            pid, up, right, down, left = map(int, fields[:5])
            pieces.append(Piece(pid, up, right, down, left))
    if len(pieces) != 256:
        raise ValueError(f"expected 256 pieces in {path}, found {len(pieces)}")
    return pieces


def rotations(piece: Piece) -> list[OrientedPiece]:
    seen: set[tuple[int, int, int, int]] = set()
    out: list[OrientedPiece] = []
    up, right, down, left = piece.up, piece.right, piece.down, piece.left
    for rot in range(4):
        key = (up, right, down, left)
        if key not in seen:
            seen.add(key)
            out.append(OrientedPiece(piece.pid, rot, up, right, down, left))
        up, right, down, left = left, up, right, down
    return out


def edge_count(piece: Piece) -> int:
    return sum(color == BORDER for color in (piece.up, piece.right, piece.down, piece.left))


def valid_on_cell(op: OrientedPiece, row: int, col: int) -> bool:
    return (
        (op.up == BORDER) == (row == 0)
        and (op.down == BORDER) == (row == BOARD - 1)
        and (op.left == BORDER) == (col == 0)
        and (op.right == BORDER) == (col == BOARD - 1)
    )


def req_matches(op: OrientedPiece, req: tuple[int, int, int, int]) -> bool:
    up, right, down, left = req
    return (
        (up == UNKNOWN or op.up == up)
        and (right == UNKNOWN or op.right == right)
        and (down == UNKNOWN or op.down == down)
        and (left == UNKNOWN or op.left == left)
    )


class Solver:
    def __init__(
        self,
        pieces: list[Piece],
        *,
        seed: int,
        hint: tuple[int, int, int] | None,
        save_dir: Path,
        report_every: float,
    ) -> None:
        self.rng = random.Random(seed)
        self.seed = seed
        self.save_dir = save_dir
        self.report_every = report_every
        self.started = time.perf_counter()
        self.last_report = self.started
        self.nodes = 0
        self.dead_ends = 0

        self.by_id = {piece.pid: piece for piece in pieces}
        self.unused = {piece.pid for piece in pieces}
        self.board: list[list[OrientedPiece | None]] = [[None for _ in range(BOARD)] for _ in range(BOARD)]
        self.best_board: list[list[OrientedPiece | None]] = [[None for _ in range(BOARD)] for _ in range(BOARD)]
        self.best_placed = 0
        self.best_score = 0

        self.color_rarity = self._build_color_rarity(pieces)
        all_orientations = [op for piece in pieces for op in rotations(piece)]
        self.cell_options: list[list[list[OrientedPiece]]] = [
            [[op for op in all_orientations if valid_on_cell(op, r, c)] for c in range(BOARD)]
            for r in range(BOARD)
        ]

        if hint is not None:
            row, col, pid = hint
            row -= 1
            col -= 1
            options = [
                op for op in rotations(self.by_id[pid])
                if valid_on_cell(op, row, col)
            ]
            if not options:
                raise ValueError(f"hint piece {pid} cannot be placed at row {row + 1}, col {col + 1}")
            self.place(row, col, options[0])

    @staticmethod
    def _build_color_rarity(pieces: Iterable[Piece]) -> dict[int, int]:
        rarity: dict[int, int] = {}
        for piece in pieces:
            for color in (piece.up, piece.right, piece.down, piece.left):
                rarity[color] = rarity.get(color, 0) + 1
        return rarity

    def placed_count(self) -> int:
        return 256 - len(self.unused)

    def place(self, row: int, col: int, op: OrientedPiece) -> None:
        self.board[row][col] = op
        self.unused.remove(op.pid)

    def remove(self, row: int, col: int, op: OrientedPiece) -> None:
        self.board[row][col] = None
        self.unused.add(op.pid)

    def requirements(self, row: int, col: int) -> tuple[int, int, int, int]:
        up = BORDER if row == 0 else UNKNOWN
        right = BORDER if col == BOARD - 1 else UNKNOWN
        down = BORDER if row == BOARD - 1 else UNKNOWN
        left = BORDER if col == 0 else UNKNOWN

        if row > 0 and self.board[row - 1][col] is not None:
            up = self.board[row - 1][col].down
        if col < BOARD - 1 and self.board[row][col + 1] is not None:
            right = self.board[row][col + 1].left
        if row < BOARD - 1 and self.board[row + 1][col] is not None:
            down = self.board[row + 1][col].up
        if col > 0 and self.board[row][col - 1] is not None:
            left = self.board[row][col - 1].right

        return up, right, down, left

    def candidates(self, row: int, col: int) -> list[OrientedPiece]:
        req = self.requirements(row, col)
        return [
            op for op in self.cell_options[row][col]
            if op.pid in self.unused and req_matches(op, req)
        ]

    def constrained_empty_cells(self) -> list[tuple[int, int]]:
        cells: list[tuple[int, int]] = []
        for row in range(BOARD):
            for col in range(BOARD):
                if self.board[row][col] is not None:
                    continue
                if (
                    row in (0, BOARD - 1)
                    or col in (0, BOARD - 1)
                    or (row > 0 and self.board[row - 1][col] is not None)
                    or (row < BOARD - 1 and self.board[row + 1][col] is not None)
                    or (col > 0 and self.board[row][col - 1] is not None)
                    or (col < BOARD - 1 and self.board[row][col + 1] is not None)
                ):
                    cells.append((row, col))
        return cells

    def choose_cell(self) -> tuple[int, int, list[OrientedPiece]] | None:
        best: tuple[int, int, list[OrientedPiece]] | None = None
        for row, col in self.constrained_empty_cells():
            cand = self.candidates(row, col)
            if not cand:
                return row, col, cand
            if best is None or len(cand) < len(best[2]):
                best = row, col, cand
                if len(cand) == 1:
                    break
        return best

    def score(self) -> int:
        total = 0
        for row in range(BOARD):
            for col in range(BOARD):
                op = self.board[row][col]
                if op is None:
                    continue
                if col + 1 < BOARD and self.board[row][col + 1] is not None:
                    total += op.right == self.board[row][col + 1].left
                if row + 1 < BOARD and self.board[row + 1][col] is not None:
                    total += op.down == self.board[row + 1][col].up
        return total

    def remember_best(self) -> None:
        placed = self.placed_count()
        score = self.score()
        if placed > self.best_placed or (placed == self.best_placed and score > self.best_score):
            self.best_placed = placed
            self.best_score = score
            self.best_board = [row.copy() for row in self.board]
            self.write_grid(self.save_dir / "best.txt")
            elapsed = time.perf_counter() - self.started
            print(
                f"best placed={placed:3d}/256 score={score:3d}/480 "
                f"nodes={self.nodes} dead={self.dead_ends} elapsed={elapsed:.1f}s",
                flush=True,
            )

    def ordered_values(self, values: list[OrientedPiece]) -> list[OrientedPiece]:
        self.rng.shuffle(values)

        def pressure(op: OrientedPiece) -> int:
            return (
                self.color_rarity.get(op.up, 0)
                + self.color_rarity.get(op.right, 0)
                + self.color_rarity.get(op.down, 0)
                + self.color_rarity.get(op.left, 0)
            )

        values.sort(key=pressure)
        return values

    def solve(self, deadline: float) -> None:
        if time.perf_counter() >= deadline:
            raise Timeout

        self.nodes += 1
        self.remember_best()

        now = time.perf_counter()
        if now - self.last_report >= self.report_every:
            self.last_report = now
            print(
                f"search placed={self.placed_count():3d} best={self.best_placed:3d} "
                f"score={self.best_score:3d} nodes={self.nodes} dead={self.dead_ends}",
                flush=True,
            )

        choice = self.choose_cell()
        if choice is None:
            return
        row, col, values = choice
        if not values:
            self.dead_ends += 1
            return

        for op in self.ordered_values(values):
            self.place(row, col, op)
            self.solve(deadline)
            self.remove(row, col, op)

    def write_grid(self, path: Path) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("w", encoding="utf-8") as handle:
            for row in self.best_board:
                handle.write("".join(f"({(op.pid if op else 0):3d} {(op.rot if op else 0):1d})" for op in row))
                handle.write("\n")


def parse_hint(value: str) -> tuple[int, int, int] | None:
    if value.lower() in {"none", "off", "false", "0"}:
        return None
    parts = value.split(",")
    if len(parts) != 3:
        raise argparse.ArgumentTypeError("hint must be row,col,piece_id or 'none'")
    row, col, pid = map(int, parts)
    if not (1 <= row <= BOARD and 1 <= col <= BOARD):
        raise argparse.ArgumentTypeError("hint row and col are 1-based and must be in 1..16")
    return row, col, pid


def main() -> int:
    parser = argparse.ArgumentParser(description="Anytime CSP solver for Eternity II.")
    parser.add_argument("--pieces", type=Path, default=Path("pieces.txt"))
    parser.add_argument("--seconds", type=float, default=60.0)
    parser.add_argument("--seed", type=int, default=None)
    parser.add_argument("--restarts", type=int, default=1)
    parser.add_argument("--save-dir", type=Path, default=Path("solver_grids"))
    parser.add_argument(
        "--hint",
        type=parse_hint,
        default=parse_hint("9,8,139"),
        help="fixed hint as row,col,piece_id using 1-based board coordinates; use 'none' to disable",
    )
    parser.add_argument("--report-every", type=float, default=10.0)
    args = parser.parse_args()

    pieces = parse_pieces(args.pieces)
    base_seed = args.seed if args.seed is not None else random.randrange(2**32)
    deadline = time.perf_counter() + args.seconds

    print(
        f"running {args.restarts} restart(s), seed={base_seed}, "
        f"limit={args.seconds:.1f}s, hint={args.hint}",
        flush=True,
    )

    best: tuple[int, int, Path] = (-1, -1, Path())
    for restart in range(args.restarts):
        if time.perf_counter() >= deadline:
            break
        solver = Solver(
            pieces,
            seed=base_seed + restart,
            hint=args.hint,
            save_dir=args.save_dir / f"restart_{restart + 1:03d}",
            report_every=args.report_every,
        )
        try:
            solver.solve(deadline)
        except Timeout:
            pass
        grid_path = solver.save_dir / "best.txt"
        if (solver.best_placed, solver.best_score) > (best[0], best[1]):
            best = solver.best_placed, solver.best_score, grid_path

    print(f"overall best placed={best[0]}/256 score={best[1]}/480 grid={best[2]}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
