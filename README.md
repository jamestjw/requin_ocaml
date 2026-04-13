# Requin

This is intended to be a rewrite of Requin, inspired in part by Stockfish. The
main point of this rewrite is to make the code as functional as possible, hence
why it's done in OCaml.

## Search Tuning

The search benchmark harness lives in `bin/bench.ml` and can be run with:

```sh
dune exec bin/bench.exe
```

Current benchmarking notes:

- The suite intentionally mixes tactical middlegames, quieter middlegames, endgames, promotions, and in-check positions.
- Search instrumentation reports fail-highs by stage, move-index bucket, and remaining depth.
- On the current suite, most fail-highs come from move index `0`, and mostly from `hash`, `good_caps`, and `killers`.

Important findings so far:

- The depth/move-index-based LMR change was a clear node-count improvement and was kept.
- A history-aware LMR tweak showed no measurable gain on the benchmark suite and was rejected.
- A naive 1-ply continuation-history implementation regressed badly, mostly due to `kiwipete`, and was rejected.
- Regressions on tactical stress positions like `kiwipete` should be treated as meaningful, not ignored as noise.

Practical rule:

- Do not keep a search heuristic just because it looks more standard or helps one or two positions; keep it only if it improves the benchmark suite overall.
