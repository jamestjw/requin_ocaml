(* Test if the engine is able to find some simple checkmates *)

open Requin
open Base
open Types
module P = Position.Position
module S = Search
module T = Types

let create_pos pieces side_to_move =
  let pos = { (P.mk ()) with side_to_move } in
  let pos =
    List.fold pieces ~init:pos ~f:(fun pos (piece, sq) -> P.put_piece pos piece sq)
  in
  let pos = P.set_state pos in
  pos
;;

let%test_unit "depth_one_white_capture_undefended_rook" =
  let pos =
    create_pos [ T.W_KING, T.E1; T.B_KING, T.E8; T.W_ROOK, T.A1; T.B_ROOK, T.A8 ] T.WHITE
  in
  let best_move = S.get_best_move pos 1 in
  assert (T.equal_move best_move @@ T.mk_move T.A8 T.A1)
;;

let%test_unit "depth_one_black_capture_undefended_rook" =
  let pos =
    create_pos [ T.W_KING, T.E1; T.B_KING, T.E8; T.W_ROOK, T.A1; T.B_ROOK, T.A8 ] T.BLACK
  in
  let best_move = S.get_best_move pos 1 in
  assert (T.equal_move best_move @@ T.mk_move T.A1 T.A8)
;;

let%test_unit "depth_two_white_win_material" =
  let pos =
    create_pos
      [ T.W_KING, T.E1
      ; T.B_KING, T.B5
      ; T.W_ROOK, T.D4
      ; T.B_BISHOP, T.A4
      ; T.B_BISHOP, T.H4
      ]
      T.WHITE
  in
  let best_move = S.get_best_move pos 2 in
  (* Capture undefended bishop *)
  assert (T.equal_move best_move @@ T.mk_move T.H4 T.D4)
;;

let%test_unit "depth_two_black_win_material" =
  let pos =
    create_pos
      [ T.W_KING, T.H1
      ; T.B_KING, T.B5
      ; T.B_ROOK, T.C2
      ; T.W_BISHOP, T.H2
      ; T.W_BISHOP, T.A2
      ]
      T.BLACK
  in
  let best_move = S.get_best_move pos 3 in
  (* Capture undefended bishop *)
  assert (T.equal_move best_move @@ T.mk_move T.A2 T.C2)
;;

let%test_unit "depth_three_white_promotes" =
  let pos =
    create_pos
      [ T.W_KING, T.F1; T.B_KING, T.A8; T.W_ROOK, T.D2; T.W_PAWN, T.E7; T.B_ROOK, T.E8 ]
      T.WHITE
  in
  let pos = P.set_state pos in
  let best_move = S.get_best_move pos 3 in
  assert (T.equal_move best_move @@ T.mk_move T.D8 T.D2)
;;

let%test_unit "white_checkmate_in_one" =
  let pos =
    P.from_fen "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 4 4"
  in
  match pos with
  | Error _ -> assert false
  | Ok pos ->
    let best_move = S.get_best_move pos 1 in
    assert (T.equal_move best_move @@ T.mk_move T.F7 T.H5)
;;

let%test_unit "black_checkmate_in_one" =
  let pos = P.from_fen "5rk1/NQp1q1pp/2B1p3/4P1p1/B1p2P2/N1P2R1K/B7/Q4nr1 b - - 0 1" in
  match pos with
  | Error _ -> assert false
  | Ok pos ->
    let best_move = S.get_best_move pos 1 in
    assert (T.equal_move best_move @@ T.mk_move T.G4 T.G5)
;;

let%test_unit "white_checkmate_in_two" =
  let pos = P.from_fen "r1bq3r/ppp1nQ2/2kp1N2/2b3n1/4P3/8/P2N1PPP/1RR3K1 w - - 0 1" in
  match pos with
  | Error _ -> assert false
  | Ok pos ->
    let best_move = S.get_best_move pos 3 in
    assert (T.equal_move best_move @@ T.mk_move T.D5 T.F7)
;;

let%test_unit "white_checkmate_in_two" =
  let pos = P.from_fen "r1bq3r/ppp1nQ2/2kp1N2/2b3n1/4P3/8/P2N1PPP/1RR3K1 w - - 0 1" in
  match pos with
  | Error _ -> assert false
  | Ok pos ->
    let best_move = S.get_best_move pos 3 in
    assert (T.equal_move best_move @@ T.mk_move T.D5 T.F7)
;;

let%test_unit "white_checkmate_in_two_v2" =
  let pos = P.from_fen "8/2p2p2/1nR4p/4k2K/1N2p2N/3p1n2/3Q2b1/b4RB1 w - - 0 1" in
  match pos with
  | Error _ -> assert false
  | Ok pos ->
    let best_move = S.get_best_move pos 3 in
    assert (T.equal_move best_move @@ T.mk_move T.E1 T.D2)
;;

let%test_unit "black mate in two" =
  let pos = P.from_fen "3r2k1/2P2p1p/4p1p1/8/3b1Q2/8/3p1RPP/3q1BK1 b - - 0 1" in
  match pos with
  | Error _ -> assert false
  | Ok pos ->
    let best_move = S.get_best_move pos 3 in
    assert (T.equal_move best_move @@ T.mk_move T.F1 T.D1)
;;

let%test_unit "white mate in three (hard)" =
  let pos = P.from_fen "7k/4K1pp/7N/8/8/8/8/B7 w - - 0 1" in
  match pos with
  | Error _ -> assert false
  | Ok pos ->
    let best_move = S.get_best_move pos 5 in
    assert (T.equal_move best_move @@ T.mk_move T.F6 T.A1)
;;

let%test_unit "black mate in three" =
  let pos = P.from_fen "8/2b2r1P/2P4k/1pK3n1/1N1R1N2/nqp5/8/8 b - - 0 1" in
  match pos with
  | Error _ -> assert false
  | Ok pos ->
    let best_move = S.get_best_move pos 5 in
    assert (T.equal_move best_move @@ T.mk_move T.F5 T.F7)
;;

let%test_unit "can search start position" =
  let pos = P.from_start_pos in
  let _ = S.get_best_move pos 5 in
  assert true
;;

let%test_unit "see_ge accepts a free rook capture" =
  let pos =
    create_pos [ T.W_KING, T.E1; T.B_KING, T.E8; T.W_ROOK, T.A1; T.B_ROOK, T.A8 ] T.WHITE
  in
  let move = T.mk_move T.A8 T.A1 in
  assert (P.see_ge pos move T.rook_value)
;;

let%test_unit "see_ge rejects thresholds above a free rook capture" =
  let pos =
    create_pos [ T.W_KING, T.E1; T.B_KING, T.E8; T.W_ROOK, T.A1; T.B_ROOK, T.A8 ] T.WHITE
  in
  let move = T.mk_move T.A8 T.A1 in
  assert (not (P.see_ge pos move T.queen_value))
;;

let%test_unit "see_ge distinguishes an even exchange from a material gain" =
  let pos =
    create_pos [ T.W_KING, T.E1; T.B_KING, T.B8; T.W_ROOK, T.A1; T.B_ROOK, T.A8 ] T.WHITE
  in
  let move = T.mk_move T.A8 T.A1 in
  assert (P.see_ge pos move 0);
  assert (not (P.see_ge pos move 1))
;;

let%test_unit "see_ge sees an x-ray rook recapture behind the captured pawn" =
  let pos =
    create_pos
      [ T.W_KING, T.G1; T.B_KING, T.G8; T.W_QUEEN, T.D1; T.B_PAWN, T.D5; T.B_ROOK, T.D8 ]
      T.WHITE
  in
  let move = T.mk_move T.D5 T.D1 in
  assert (not (P.see_ge pos move 0))
;;

let%test_unit "see_ge ignores a pinned recapturing knight" =
  let pos =
    create_pos
      [ T.W_KING, T.G1
      ; T.B_KING, T.E8
      ; T.W_ROOK, T.E1
      ; T.W_ROOK, T.C1
      ; T.B_KNIGHT, T.E7
      ; T.B_BISHOP, T.C8
      ]
      T.WHITE
  in
  let move = T.mk_move T.C8 T.C1 in
  assert (P.see_ge pos move 100)
;;

let%test_unit "see_ge falls back to the simple threshold check on promotions" =
  let pos = create_pos [ T.W_KING, T.H1; T.B_KING, T.A8; T.W_PAWN, T.E7 ] T.WHITE in
  let move = T.mk_move ~move_type:T.PROMOTION ~ppt:(Some T.QUEEN) T.E8 T.E7 in
  (* Non-normal moves currently bypass full SEE and use the simple 0 >= threshold fallback. *)
  assert (P.see_ge pos move 0);
  assert (not (P.see_ge pos move 1))
;;

let%test_unit "pseudo_legal accepts kingside castling when available" =
  let pos = P.from_fen "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1" |> Stdlib.Result.get_ok in
  let move = T.mk_move ~move_type:T.CASTLING T.H1 T.E1 in
  assert (P.pseudo_legal pos move)
;;

let%test_unit "pseudo_legal accepts queen promotion push" =
  let pos = create_pos [ T.W_KING, T.H1; T.B_KING, T.A8; T.W_PAWN, T.E7 ] T.WHITE in
  let move = T.mk_move ~move_type:T.PROMOTION ~ppt:(Some T.QUEEN) T.E8 T.E7 in
  assert (P.pseudo_legal pos move)
;;

let%test_unit "pseudo_legal rejects impossible en passant capture" =
  let pos = P.from_fen "4k3/8/8/3pP3/8/8/8/4K3 w - - 0 1" |> Stdlib.Result.get_ok in
  let move = T.mk_move ~move_type:T.EN_PASSANT T.D6 T.E5 in
  assert (not (P.pseudo_legal pos move))
;;

let%test_unit "capture_order_score prefers more valuable victims" =
  let pos =
    create_pos
      [ T.W_KING, T.G1
      ; T.B_KING, T.G8
      ; T.W_KNIGHT, T.E4
      ; T.B_PAWN, T.C5
      ; T.B_QUEEN, T.F6
      ]
      T.WHITE
  in
  let take_pawn = T.mk_move T.C5 T.E4 in
  let take_queen = T.mk_move T.F6 T.E4 in
  assert (S.capture_order_score pos take_queen > S.capture_order_score pos take_pawn)
;;

let%test_unit "capture_order_score prefers cheaper attackers for the same victim" =
  let pos =
    create_pos
      [ T.W_KING, T.G1; T.B_KING, T.G8; T.W_PAWN, T.E4; T.W_QUEEN, T.A4; T.B_QUEEN, T.D5 ]
      T.WHITE
  in
  let pawn_takes_queen = T.mk_move T.D5 T.E4 in
  let queen_takes_queen = T.mk_move T.D5 T.A4 in
  assert (
    S.capture_order_score pos pawn_takes_queen
    > S.capture_order_score pos queen_takes_queen)
;;

let%test_unit "capture_order_score gives promotions an extra boost" =
  let pos =
    create_pos
      [ T.W_KING, T.H1; T.B_KING, T.A8; T.W_PAWN, T.E7; T.W_QUEEN, T.C4; T.B_PAWN, T.D6 ]
      T.WHITE
  in
  let promote = T.mk_move ~move_type:T.PROMOTION ~ppt:(Some T.QUEEN) T.E8 T.E7 in
  let capture_pawn = T.mk_move T.D6 T.C4 in
  assert (S.capture_order_score pos promote > S.capture_order_score pos capture_pawn)
;;
