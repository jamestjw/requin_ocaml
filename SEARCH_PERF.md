# Search Performance Improvement Backlog

Concrete, codebase-specific opportunities to improve search NPS / elo,
ranked by impact / effort. All file:line references are against `main`
at the time of writing.

## Top wins

### 1. Cache static eval in `state_info`
- **Where**: `lib/search.ml:431` (qsearch), `lib/search.ml:553` (main search)
- **Today**: `Eval.evaluate pos ()` is recomputed on every TT-miss node and
  every qsearch node. At ~50% TT hit rate this is ~5M eval calls per 10M
  nodes.
- **Change**: Compute eval once at node entry, store on `state_info` (and
  in TT entries — see #8). Restore on TT hit.
- **Impact**: ~5–8% NPS
- **Effort**: small (2–3h)

### 2. Memoize recomputed Maps in `set_check_info`
- **Where**: `lib/position.ml:1139` (`new_st_from_prev`), `lib/position.ml:1443`
- **Today**: `new_st_from_prev` allocates 7 fresh `Map.empty` fields on every
  `do_move'` (`blockers_for_king`, `pinners`, `check_squares`, …), then
  `set_check_info` recomputes them. Several of those Maps are pure
  overhead at most nodes.
- **Change**: Drop or lazy-compute the Maps that are not needed at every
  node; reuse computation where possible.
- **Impact**: ~3–5% NPS
- **Effort**: small. Also a useful stepping stone toward full make/unmake (#9).

### 3. Mutable move buffer instead of `Types.move list`
- **Where**: `lib/movegen.ml:128–248`, `lib/search.ml:63–67`,
  `lib/search.ml:306–344`
- **Today**: Every node builds 5–15 cons-cell lists (pawn pushes, captures,
  quiets, then `List.map` / `List.filter` / sort). Hundreds of MB of
  transient allocations at 10M nodes.
- **Change**: Switch the move picker to a `move array` with a length
  counter, sort in-place. Movegen functions take a buffer + index instead
  of returning lists.
- **Impact**: ~8–15% NPS
- **Effort**: medium (4–6h). Touches movegen API; perft is the safety net.

### 4. Logarithmic LMR formula
- **Where**: `lib/search.ml:27–31`
- **Today**: `1 + (remaining_depth - 3)/2 + (move_index - 3)/4`. Reduces too
  little at shallow depths, too much at deep ones; no adaptation to
  fail-high history.
- **Change**: Standard `log(depth) · log(move_index)` shaped table, capped
  at ~8 plies.
- **Impact**: ~2–4% elo-equivalent
- **Effort**: ~30 min

### 5. Quiet-move ordering by depth-weighted history + killer bonus
- **Where**: `lib/search.ml:330–344` (`get_quiets`)
- **Today**: Quiets sorted by raw history value only. No depth scaling, no
  killer bonus weighting, no countermove ordering.
- **Change**: Multiply history by `remaining_depth / 4`; add explicit
  `+1000 * (1 + killer_index)` for killers; consider countermove table.
- **Impact**: ~3–5% NPS (earlier beta cutoffs on quiet branches)
- **Effort**: small (~2h)

## Medium wins

### 6. Depth-scaled futility / reverse-futility margin
- **Where**: `lib/search.ml:24–25`, `lib/search.ml:797–805`
- **Today**: Fixed margins at depth 1 (bishop value) and depth 2 (rook +
  knight). Nothing for depth ≥ 3.
- **Change**: `margin = 50 + 100 * depth` (or tuned equivalent).
- **Impact**: ~1–2% NPS
- **Effort**: trivial

### 7. Killer + history micro-allocations
- **Where**: `lib/killer.ml:8–15`, `lib/search.ml:339, 654, 700`
- **Today**: `Killer.get_killers` rebuilds a list every node;
  `History.get` is a `Hashtbl.find` per quiet move.
- **Change**: Killers as a fixed-size array (no allocation per access);
  history as `int array` indexed by `(piece, from, to)`.
- **Impact**: ~1–3% combined
- **Effort**: ~3h

### 8. TT-aware static eval store/load
- **Where**: `lib/transposition_table.ml`, plus search probe/store sites
- **Today**: Eval not stored in TT; iterative deepening re-search recomputes.
- **Change**: Add `eval` slot to TT entries; write on store, read on hit.
- **Impact**: ~2–3% on top of #1
- **Effort**: ~2h (do after #1)

## Bigger lifts

### 9. Make/unmake on `Position`
- **Where**: `lib/position.ml:1186–1530`
- **Today**: `do_move'` returns a fresh `Position.t`; the entire `state_info`
  record (large — `material_key`, `pawn_key`, `psq_score`, 4 maps, 3
  bitboards, `previous` pointer, 8 scalars) is allocated per ply. ~10M
  state record allocations per 10M-node search.
- **Change**: In-place mutation + undo log. Major refactor of every
  call-site, but eliminates the dominant per-node allocation.
- **Impact**: ~15–25% NPS
- **Effort**: large (15+ hours). Perft must stay green throughout.

### 10. Lazy SMP via `domainslib`
- **Where**: not yet wired (already declared in `dune-project`)
- **Today**: Single-threaded.
- **Change**: Lazy SMP — multiple search workers, shared TT, root split.
- **Impact**: roughly linear to ~4 cores, then sublinear
- **Effort**: large

## Recommended order

Each as its own commit so the bench harness can attribute the win:

1. **Static eval cache** (#1)
2. **Logarithmic LMR** (#4)
3. **Quiet ordering** (#5)
4. **Memoize check-info Maps** (#2)
5. **TT eval slot** (#8) — pairs with #1
6. **Depth-scaled futility** (#6)
7. **Killer/history allocations** (#7)
8. **Move buffer rewrite** (#3) — biggest single allocation win; do after
   smaller wins so you can isolate its effect.

Items 1–7 should net ~15–20% NPS plus measurable elo at <20h total.
Items 9 and 10 are project-scale undertakings; defer until the smaller
wins are exhausted.

## Workflow notes

- Use `dune exec bin/bench.exe --profile=release` before/after each change
  to measure NPS and node-count deltas. Compare on the same machine.
- Use `dune exec bin/perft.exe -- suite 4` after any change that touches
  movegen, position, or move execution.
- Treat any change that loses NPS *and* loses on the bench suite as a
  revert. Per the README, individual-position improvements that don't
  carry the suite are not kept.
