(* Test if the engine is able to find some simple checkmates *)

open Requin
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

let%test_unit "depth_one_white_capture_undefended_rook" =
  let pos = { (P.mk ()) with side_to_move = T.WHITE } in
  P.put_piece pos T.W_KING T.E1;
  P.put_piece pos T.B_KING T.E8;
  P.put_piece pos T.W_ROOK T.A1;
  P.put_piece pos T.B_ROOK T.A8;
  let best_move = S.get_best_move pos 1 0 in
  assert (T.equal_move best_move @@ T.mk_move T.A8 T.A1)
;;

let%test_unit "depth_one_black_capture_undefended_rook" =
  let pos = { (P.mk ()) with side_to_move = T.BLACK } in
  P.put_piece pos T.W_KING T.E1;
  P.put_piece pos T.B_KING T.E8;
  P.put_piece pos T.W_ROOK T.A1;
  P.put_piece pos T.B_ROOK T.A8;
  let best_move = S.get_best_move pos 1 0 in
  assert (T.equal_move best_move @@ T.mk_move T.A1 T.A8)
;;

let%test_unit "depth_two_white_win_material" =
  let pos = { (P.mk ()) with side_to_move = T.WHITE } in
  P.put_piece pos T.W_KING T.E1;
  P.put_piece pos T.B_KING T.B5;
  P.put_piece pos T.W_ROOK T.D4;
  P.put_piece pos T.B_BISHOP T.A4;
  P.put_piece pos T.B_BISHOP T.H4;
  let best_move = S.get_best_move pos 2 0 in
  (* Capture undefended bishop *)
  assert (T.equal_move best_move @@ T.mk_move T.H4 T.D4)
;;

let%test_unit "depth_two_black_win_material" =
  let pos = { (P.mk ()) with side_to_move = T.BLACK } in
  P.put_piece pos T.W_KING T.H1;
  P.put_piece pos T.B_KING T.B5;
  P.put_piece pos T.B_ROOK T.C2;
  P.put_piece pos T.W_BISHOP T.H2;
  P.put_piece pos T.W_BISHOP T.A2;
  let best_move = S.get_best_move pos 2 0 in
  (* Capture undefended bishop *)
  assert (T.equal_move best_move @@ T.mk_move T.A2 T.C2)
;;
