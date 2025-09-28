(* Test if the engine is able to find some simple checkmates *)

open Base
open Types
module P = Position.Position
module S = Search
module T = Types

(* EASY: Scholar's mate *)
let%test_unit "checkmate_in_one" =
  let pos =
    P.from_fen "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 4 4"
  in
  match pos with
  | Error _ -> assert false
  | Ok pos ->
    let best_move = S.get_best_move pos 1 0 in
    assert (T.equal_move best_move @@ T.mk_move T.F7 T.H5)
;;
