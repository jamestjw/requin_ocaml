open Base
open Requin
module P = Position.Position

(* Standard perft positions with known node counts. Depths kept shallow so the
   suite finishes quickly in CI; deeper validation is available via
   `dune exec bin/perft.exe -- suite 5`. *)

let check name fen depth expected =
  let pos = P.from_fen fen |> Stdlib.Result.get_ok in
  let got = Perft.perft pos depth in
  if got <> expected
  then
    Printf.failwithf
      "perft mismatch for %s at depth %d: got %d, want %d"
      name
      depth
      got
      expected
      ()
;;

let%test_unit "perft startpos depth 1" =
  check "startpos" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 1 20
;;

let%test_unit "perft startpos depth 2" =
  check "startpos" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 2 400
;;

let%test_unit "perft startpos depth 3" =
  check "startpos" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 3 8902
;;

let%test_unit "perft startpos depth 4" =
  check "startpos" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" 4 197281
;;

let%test_unit "perft kiwipete depth 1" =
  check
    "kiwipete"
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    1
    48
;;

let%test_unit "perft kiwipete depth 2" =
  check
    "kiwipete"
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    2
    2039
;;

let%test_unit "perft kiwipete depth 3" =
  check
    "kiwipete"
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
    3
    97862
;;

let%test_unit "perft position3 depth 4" =
  check "position3" "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1" 4 43238
;;

let%test_unit "perft position4 depth 3" =
  check
    "position4"
    "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"
    3
    9467
;;

let%test_unit "perft position4_mirror depth 3" =
  check
    "position4_mirror"
    "r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1"
    3
    9467
;;

let%test_unit "perft position5 depth 3" =
  check "position5" "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8" 3 62379
;;

let%test_unit "perft position6 depth 3" =
  check
    "position6"
    "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"
    3
    89890
;;
