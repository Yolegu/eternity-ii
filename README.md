# eternity-ii

Brute force and constraint-search experiments for the Eternity II puzzle.

## Python CSP solver

Run an anytime constraint solver:

```powershell
py -3 e2_solver.py --seconds 300 --restarts 20
```

The solver uses the existing `pieces.txt`, fixes the official hint piece 139 at
row 9, column 8 by default, and writes the best grid for each restart under
`solver_grids/`. Disable the hint with:

```powershell
py -3 e2_solver.py --hint none --seconds 300
```

## Full-board optimizer

Run the conflict-directed annealer that always places all 256 pieces and tries
to maximize the 480 internal matches:

```powershell
py -3 e2_full_solver.py --seconds 3600 --restart-steps 500000 --sample-size 48
```

It writes the best complete board to `full_solver_grids/best_full.txt`. A
perfect solution is reported when the score reaches `480/480`.

Resume from a saved full board:

```powershell
py -3 e2_full_solver.py --start-grid full_solver_grids\best_full.txt --seconds 3600
```

Run independent annealers in parallel:

```powershell
py -3 e2_full_solver.py --workers 8 --seconds 3600 --sample-size 64
```

When the run is stuck at a high score, switch to plateau polishing. This keeps
the current best board, scans deterministic conflict swaps, and repairs 4x4
patches around bad edges:

```powershell
py -3 e2_full_solver.py --seconds 3600 --patch-size 4 --patch-beam 1500 --patch-rate 0.18 --exhaustive-rate 0.30 --exhaustive-cells 8 --temp-start 0.25 --temp-end 0.005
```

## Polynomial-budget mode

An exact polynomial-time solver for Eternity II is not expected: the underlying
edge-matching decision problem is NP-complete. The `--poly-passes` mode instead
gives a polynomial-bounded heuristic run. For fixed `--patch-size` and
`--patch-beam`, it performs `passes * n^2` moves and the targeted exhaustive
swap makes the bound `O(passes * n^3)` for `n` pieces.

```powershell
py -3 e2_full_solver.py --poly-passes 20 --patch-size 4 --patch-beam 1500 --patch-rate 0.18 --exhaustive-rate 0.30 --exhaustive-cells 8 --temp-start 0.25 --temp-end 0.005
```

## Assignment-relaxation mode

This is the most polynomial-like optimizer in the repo. Each pass freezes the
current neighbor colors, scores every legal piece/orientation for every cell,
and solves the resulting global assignment with the Hungarian algorithm in
`O(n^3)`. This can move many pieces coherently, unlike local swaps.

For a high plateau such as `424/480`, start from the best saved grid:

```powershell
py -3 e2_full_solver.py --start-grid full_solver_grids\best_full.txt --relax-passes 200 --relax-polish 512 --relax-inertia 25 --relax-jitter 4 --relax-max-drop 2 --patch-size 4 --patch-beam 1200 --patch-rate 0.10 --exhaustive-rate 0.25 --exhaustive-cells 8 --temp-start 0.16 --temp-end 0.004
```

## Hard plateau mode

If every direct patch is stuck, use swap-chain and basin-hop moves. This mode
allows a bounded temporary score drop, then repairs from the perturbed state.

```powershell
py -3 e2_full_solver.py --start-grid full_solver_grids\best_full.txt --basin-jumps 200 --shake-size 24 --shake-drop 58 --shake-tries 80 --shake-polish 1200 --chain-rate 0.45 --chain-depth 4 --chain-beam 48 --chain-first 12 --chain-drop 5 --pool-size 48 --pool-rate 0.12 --line-rate 0.12 --line-length 12 --line-beam 6000 --patch-rate 0.04 --exhaustive-rate 0.25 --exhaustive-cells 12 --temp-start 0.65 --temp-end 0.03
```
