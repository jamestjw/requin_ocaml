open Base
open Bitboard
open Types
module BB = Bitboard
module P = Position.Position

module MoveGen = struct
  type gen_type =
    | CAPTURES (* Generates all pseudo-legal captures plus queen promotions *)
    | QUIETS (* Generates all pseudo-legal non-captures and underpromotions *)
    (* Generates all pseudo-legal non-captures giving check,
       except castling and promotions *)
    | QUIET_CHECKS
    | EVASIONS (* Generates all pseudo-legal check evasions *)
    | NON_EVASIONS (* Generates all pseudo-legal captures and non-captures *)
    | LEGAL (* Generates all the legal moves in the given position *)
  [@@deriving eq]

  (* type move_list = Types.move list *)

  (* `enemy` means that we are capturing a piece while promoting *)
  (* gt:
      - QUIETS -> We only generate non-queen promotions without capturing
      enemy pieces
      - CAPTURES -> We always generate the queen promotion, but for the
      other promotions we only generate them when the destination square
      is indeed a capture.
      - EVASIONS | NON_EVASIONS -> We assume that the destination square
      is always legit, and we generate all the promotions to that square.
      _ _others_ -> We generate nothing. *)
  (* TODO: Why are we passing in the move_list here? Just return a new list? *)
  let mk_promotions gt dir enemy move_list dst =
    let src = Types.sq_sub_dir dst dir |> Stdlib.Option.get in
    let all =
      match gt with
      | EVASIONS | NON_EVASIONS -> true
      | _ -> false
    in
    let move_list =
      if equal_gen_type gt CAPTURES || all
      then Types.mk_move ~move_type:Types.PROMOTION ~ppt:Types.QUEEN dst src :: move_list
      else move_list
    in
    let move_list =
      if
        (equal_gen_type gt CAPTURES && enemy)
        || (equal_gen_type gt QUIETS && not enemy)
        || all
      then
        Types.mk_move ~move_type:Types.PROMOTION ~ppt:Types.ROOK dst src
        :: Types.mk_move ~move_type:Types.PROMOTION ~ppt:Types.BISHOP dst src
        :: Types.mk_move ~move_type:Types.PROMOTION ~ppt:Types.KNIGHT dst src
        :: move_list
      else move_list
    in
    move_list
  ;;

  let generate_pawn_moves us gt pos target =
    let them = Types.other_colour us in
    let rank7_bb, rank3_bb =
      match us with
      | Types.WHITE -> BB.rank_7, BB.rank_3
      | Types.BLACK -> BB.rank_2, BB.rank_6
    in
    let up = Types.pawn_push_direction us in
    let up_right, up_left =
      match us with
      | Types.WHITE -> Types.NORTH_EAST, Types.NORTH_WEST
      | Types.BLACK -> Types.SOUTH_WEST, Types.SOUTH_EAST
    in
    let empty_squares = BB.bb_not @@ P.pieces pos in
    (* The pieces that we consider to be the enemies, if we are simply
       evading checks, then only the checkers are the enemies. *)
    let enemies =
      match gt with
      | EVASIONS -> P.checkers pos
      | _ -> P.pieces_of_colour pos them
    in
    let our_pawns = P.pieces_of_colour_and_pt pos us Types.PAWN in
    let pawns_on_7th = BB.bb_and our_pawns rank7_bb in
    let pawns_not_on_7th = BB.bb_and our_pawns @@ BB.bb_not rank7_bb in
    (* Single and double pawn pushes, no promotions *)
    let res =
      if not @@ equal_gen_type gt CAPTURES
      then (
        (* Single advance *)
        let b1 = BB.shift pawns_not_on_7th up |> BB.bb_and empty_squares in
        (* Double advance (only include pawns that are on the 3th rank after
           advancing once) *)
        let b2 = BB.shift (BB.bb_and b1 rank3_bb) up |> BB.bb_and empty_squares in
        let b1, b2 =
          match gt with
          (* If we are evading checks, we only consider the pawn moves that
             defend against checks (passed in as `target`)*)
          | EVASIONS -> BB.bb_and b1 target, BB.bb_and b2 target
          (* To make a quiet check, you either make a direct check by pushing a pawn
             or push a blocker pawn that is not on the same file as the enemy king. *)
          | QUIET_CHECKS ->
            let their_king_sq = P.square_of_pt_and_colour pos Types.KING them in
            (* Discovered check candidates *)
            let dc_candidates =
              P.blockers_for_king pos them
              |> BB.bb_and @@ BB.bb_not @@ BB.file_bb_of_sq their_king_sq
            in
            (* Pawns that either directly attack the king after advancing once
                 or are part of the `dc_candidates` *)
            let b1 =
              BB.bb_and
                b1
                (BB.bb_or
                   (BB.pawn_attacks_bb_from_sq them their_king_sq)
                   (BB.shift dc_candidates up))
            in
            let b2 =
              BB.bb_and
                b2
                (BB.bb_or
                   (BB.pawn_attacks_bb_from_sq them their_king_sq)
                   (BB.shift (BB.shift dc_candidates up) up))
            in
            b1, b2
          | _ -> b1, b2
        in
        let res =
          BB.fold
            ~init:[]
            ~f:(fun acc dst_bb ->
              let dst_sq = BB.bb_to_square dst_bb in
              let src_sq = Stdlib.Option.get (Types.sq_sub_dir dst_sq up) in
              Types.mk_move dst_sq src_sq :: acc)
            b1
        in
        BB.fold
          ~init:res
          ~f:(fun acc dst_bb ->
            let dst_sq = BB.bb_to_square dst_bb in
            let src_sq = Stdlib.Option.get (Types.sq_sub_dir_twice dst_sq up) in
            Types.mk_move dst_sq src_sq :: acc)
          b2)
      else []
    in
    (* Promotions and underpromotions (only use pawns on the 7th) *)
    let res =
      if BB.bb_not_zero pawns_on_7th
      then (
        (* Three ways to promote, either we capture to the left/right, or we
           simply advance the pawn. *)
        let b1 = BB.shift pawns_on_7th up_right |> BB.bb_and enemies in
        let b2 = BB.shift pawns_on_7th up_left |> BB.bb_and enemies in
        let b3 = BB.shift pawns_on_7th up |> BB.bb_and empty_squares in
        (* In EVASIONS mode, we are obliged to block the check. Note that
           for `b1` and `b2`, we would already have set enemies to the checkers
           for EVASIONS mode earlier on. *)
        let b3 =
          match gt with
          | EVASIONS -> BB.bb_and b3 target
          | _ -> b3
        in
        (* Generate up_right captures *)
        let res =
          BB.fold
            ~init:res
            ~f:(fun acc bb ->
              let sq = BB.bb_to_square bb in
              mk_promotions gt up_right true acc sq)
            b1
        in
        (* Generate up_left captures *)
        let res =
          BB.fold
            ~init:res
            ~f:(fun acc bb ->
              let sq = BB.bb_to_square bb in
              mk_promotions gt up_left true acc sq)
            b2
        in
        (* Generate advances
           NOTE: We pass in `false` as we know this isn't a capture *)
        BB.fold
          ~init:res
          ~f:(fun acc bb ->
            let sq = BB.bb_to_square bb in
            mk_promotions gt up false acc sq)
          b3)
      else res
    in
    (* Standard and en passant captures, only use pawns not on the 7th *)
    match gt with
    | CAPTURES | EVASIONS | NON_EVASIONS ->
      let b1 = BB.shift pawns_not_on_7th up_right |> BB.bb_and enemies in
      let b2 = BB.shift pawns_not_on_7th up_left |> BB.bb_and enemies in
      (* Generate up_right captures *)
      let res =
        BB.fold
          ~init:res
          ~f:(fun acc bb ->
            let dst_sq = BB.bb_to_square bb in
            let src_sq = Stdlib.Option.get (Types.sq_sub_dir dst_sq up_right) in
            Types.mk_move dst_sq src_sq :: acc)
          b1
      in
      (* Generate up_left captures *)
      let res =
        BB.fold
          ~init:res
          ~f:(fun acc bb ->
            let dst_sq = BB.bb_to_square bb in
            let src_sq = Stdlib.Option.get (Types.sq_sub_dir dst_sq up_left) in
            Types.mk_move dst_sq src_sq :: acc)
          b2
      in
      (* Handle en passant captures *)
      (match P.ep_square pos with
       | Some ep_square ->
         assert (Types.equal_rank (Types.relative_rank_of_sq us ep_square) Types.RANK_6);
         (* See if the en passant capture cannot defend against a check *)
         if
           (* If the enemy pawn that advanced two squares gave a discovered
                 check, then the square that we need to occupy to defend
                 against the check was the pawn's original square is
                 ep_square + up. *)
           equal_gen_type EVASIONS gt
           && (BB.bb_not_zero
               @@ BB.bb_and_sq target (Stdlib.Option.get @@ Types.sq_plus_dir ep_square up)
              )
         then res (* Nothing else to do *)
         else (
           (* Pawns that can capture en passant are precisely the ones
                 that may 'capture' the landing square of the en passant
                 capture. *)
           let b1 =
             BB.bb_and pawns_not_on_7th @@ BB.pawn_attacks_bb_from_sq them ep_square
           in
           assert (BB.bb_not_zero b1);
           (* There may be multiple pawns that may capture en passant,
                 imagine a black pawn on D5 and white pawns on C5 and E5. *)
           BB.fold
             ~init:res
             ~f:(fun acc bb ->
               let sq = BB.bb_to_square bb in
               Types.mk_move ~move_type:Types.EN_PASSANT ep_square sq :: acc)
             b1)
       | None -> res)
    | _ -> res
  ;;

  (* Generate non-king and non-pawn moves *)
  let generate_moves us pt checks pos target =
    let occupancy = P.pieces pos in
    let their_king_blockers = P.blockers_for_king pos @@ Types.other_colour us in
    let pt_check_squares = P.check_squares pos pt in
    let do_bb acc bb =
      let src_sq = BB.bb_to_square bb in
      let moves_bb = BB.attacks_bb_occupied pt src_sq occupancy |> BB.bb_and target in
      (* To check, you either move freely a blocker or make a direct check. *)
      let moves_bb =
        if
          checks
          (* TODO: What's this special treatment of the queen? This is odd
             since a queen is never a blocker anyway *)
          (* If the piece is not a king blocker, then it won't be the case
             that any random move would give check, hence we filter out the
             moves.*)
          && (Types.equal_piece_type pt Types.QUEEN
              || BB.bb_is_empty (BB.bb_and_sq their_king_blockers src_sq))
        then BB.bb_and moves_bb pt_check_squares
        else moves_bb
      in
      BB.fold_sq
        ~init:acc
        ~f:(fun acc dst_sq -> Types.mk_move dst_sq src_sq :: acc)
        moves_bb
    in
    (match pt with
     | Types.KING | Types.PAWN -> failwith "Unsupported piece type"
     | _ -> ());
    BB.fold ~init:[] ~f:do_bb (P.pieces_of_colour_and_pt pos us pt)
  ;;

  let generate_all us gt pos =
    assert (not @@ equal_gen_type gt LEGAL);
    let them = Types.other_colour us in
    let checks = equal_gen_type gt QUIET_CHECKS in
    let king_sq = P.square_of_pt_and_colour pos Types.KING us in
    let checkers = P.checkers pos in
    let res, target =
      (* Skip generating non-king moves when in double check *)
      if equal_gen_type gt EVASIONS && (BB.more_than_one @@ checkers)
      then [], BB.empty
      else (
        let target =
          match gt with
          (* We know that there is one checker, just interpose or capture the
             checker*)
          | EVASIONS -> BB.between_bb king_sq @@ BB.bb_to_square @@ BB.lsb checkers
          | NON_EVASIONS -> BB.bb_not @@ P.pieces_of_colour pos us
          | CAPTURES -> P.pieces_of_colour pos them
          | QUIETS | QUIET_CHECKS -> BB.bb_not @@ P.pieces pos
          | LEGAL -> failwith "impossible"
        in
        let res = generate_pawn_moves us gt pos target in
        let res = generate_moves us Types.KNIGHT checks pos target @ res in
        let res = generate_moves us Types.BISHOP checks pos target @ res in
        let res = generate_moves us Types.ROOK checks pos target @ res in
        let res = generate_moves us Types.QUEEN checks pos target @ res in
        res, target)
    in
    (* Handle king moves. We skip move generation if we need to do a check
       but our king is not a blocker. *)
    (* TODO: what about castling into check? *)
    if
      (not checks)
      || (BB.bb_not_zero @@ BB.sq_and_bb king_sq @@ P.blockers_for_king pos them)
    then (
      (* If we are evading a check, move the king to a square that we do not
         occupy. Seems like we do not verify that the destination square is
         safe, i.e. we expect the caller to filter out illegal king moves. *)
      let bb =
        BB.attacks_bb Types.KING king_sq
        |> BB.bb_and
             (if equal_gen_type gt EVASIONS
              then BB.bb_not (P.pieces_of_colour pos us)
              else target)
      in
      (* To do a discovery check with the king, just move the king out of an
         alignment with the enemy king. *)
      let bb =
        if checks
        then
          BB.bb_and
            bb
            (BB.bb_not
             @@ BB.attacks_bb Types.QUEEN (P.square_of_pt_and_colour pos Types.KING them)
            )
        else bb
      in
      let res =
        BB.fold_sq ~init:res ~f:(fun acc dst -> Types.mk_move dst king_sq :: acc) bb
      in
      (* Handle castling *)
      if equal_gen_type gt QUIETS || equal_gen_type gt NON_EVASIONS
      then
        List.fold
          ~init:res
          ~f:(fun acc cr ->
            if (not (P.castling_impeded pos cr)) && P.can_castle pos cr
            then
              Types.mk_move
                ~move_type:Types.CASTLING
                (Stdlib.Option.get @@ P.castling_rook_square pos cr)
                king_sq
              :: acc
            else acc)
          (Types.castling_rights_for_colour us)
      else res)
    else res
  ;;

  (* Check the definition of `gen_type` to see how it influences the moves
     produced by this function. *)
  let rec generate_type gt pos =
    assert (not @@ equal_gen_type LEGAL gt);
    assert (Bool.equal (equal_gen_type EVASIONS gt) (BB.bb_not_zero @@ P.checkers pos));
    generate_all (P.side_to_move pos) gt pos

  and generate_legal pos =
    let us = P.side_to_move pos in
    let pinned = P.blockers_for_king pos us |> BB.bb_and @@ P.pieces_of_colour pos us in
    let king_sq = P.square_of_pt_and_colour pos Types.KING us in
    let is_legal m =
      let src_sq = Types.move_src m in
      not (* The following conditions give us a move that is not legal *)
      @@ (((* Moving a pinned piece *)
           (BB.bb_not_zero @@ BB.sq_and_bb src_sq pinned)
           (* Moving the king*)
           || Types.equal_square src_sq king_sq
           (* Doing en passant *)
           || Types.equal_move_type (Types.get_move_type m) Types.EN_PASSANT)
          (* But is not legal *)
          && (not @@ P.legal pos m))
    in
    let move_list =
      if BB.bb_not_zero @@ P.checkers pos
      then generate EVASIONS pos
      else generate NON_EVASIONS pos
    in
    List.filter move_list ~f:is_legal

  and generate gt pos =
    match gt with
    | LEGAL -> generate_legal pos
    | _ -> generate_type gt pos
  ;;
end
