open Base
open Bitboard
open Types
open Unsigned
module BB = Bitboard

module Position = struct
  type key = UInt64.t

  (*
     * StateInfo struct stores information needed to restore a Position object to
   * its previous state when we retract a move. Whenever a move is made on the
   * board (by calling Position::do_move), a StateInfo object must be passed.
  *)
  type state_info =
    { (*
         * These fields are copied when making a move
      *)
      material_key : key
    ; pawn_key : key
    ; non_pawn_material : Types.value array
    ; castling_rights : int
    ; (* 50-move rule counter, draw if this reaches 100 as we count in plies *)
      rule50 : int
    ; plies_from_null : int (* TODO: What's this? *)
    ; (* En passant square, i.e. the square that the enemy pawn will land on
       after capturing en passant. This conveniently handles the case when
       two possible pawns could capture en passant.*)
      ep_square : Types.square option
    ; (*
         * Below are not copied when making a move (will be recomputed anyhow)
      *)
      key : key
    ; checkers_bb : BB.t
    ; previous : state_info option
    ; blockers_for_king : BB.t array
    ; pinners : BB.t array
    ; (* Squares checked by each piece type *)
      check_squares : BB.t array
    ; captured_piece : Types.piece option
    ; repetition : int
    }

  type t =
    { board : Types.piece option array
    ; by_type_bb : BB.t array
    ; by_colour_bb : BB.t array
    ; piece_count : int array
    ; (* TODO: Verify this
       Every square potentially contains a piece that is involved in
       some castling right. The int is the bitwise-or of all the
       involved castling rights. This means that when a piece on that
       square is moved or captured, then the value in the array is
       precisely the change in castling rights. *)
      castling_rights_mask : int Map.M(SquareCmp).t
    ; castling_rook_square : (Types.castling_right, Types.square) Hashtbl.t
    ; castling_path : (Types.castling_right, BB.t) Hashtbl.t
    ; game_ply : int
    ; side_to_move : Types.colour
    ; chess960 : bool
    ; st : state_info
    }

  let show { board; _ } =
    let res =
      List.fold
        ~init:"+---+---+---+---+---+---+---+---+\n"
        ~f:(fun acc rank ->
          let res =
            List.fold
              ~init:acc
              ~f:(fun acc file ->
                let to_add =
                  match
                    Array.get board (Types.mk_square ~file ~rank |> Types.square_to_enum)
                  with
                  | Some piece -> Printf.sprintf "| %s " (Types.short_piece_name piece)
                  | None -> "|   "
                in
                acc ^ to_add)
              Types.all_files
          in
          res
          ^ Printf.sprintf
              "| %d\n\n+---+---+---+---+---+---+---+---+\n"
              (Types.rank_to_enum rank + 1))
        (List.rev Types.all_ranks)
    in
    res ^ "  a   b   c   d   e   f   g   h\n"
  ;;

  let side_to_move { side_to_move; _ } = side_to_move
  let is_white_to_move pos = Types.equal_colour (side_to_move pos) Types.WHITE
  let piece_on { board; _ } sq = Array.get board (Types.square_to_enum sq)
  let piece_on_exn pos sq = piece_on pos sq |> Stdlib.Option.get

  let piece_type_on_exn pos sq =
    piece_on pos sq |> Stdlib.Option.get |> Types.type_of_piece
  ;;

  let is_empty pos sq = piece_on pos sq |> Option.is_none
  let moved_piece pos m = piece_on pos @@ Types.move_src m
  let moved_piece_exn pos m = moved_piece pos m |> Stdlib.Option.get

  let pieces_of_pt { by_type_bb; _ } pt =
    Array.get by_type_bb (Types.piece_type_to_enum pt)
  ;;

  let pieces_of_pts pos pts =
    List.fold ~init:BB.empty ~f:(fun acc pt -> BB.bb_or acc @@ pieces_of_pt pos pt) pts
  ;;

  let pieces_of_colour { by_colour_bb; _ } colour =
    Array.get by_colour_bb (Types.colour_to_enum colour)
  ;;

  (* Get all pieces *)
  let pieces pos =
    BB.bb_or (pieces_of_colour pos Types.WHITE) (pieces_of_colour pos Types.BLACK)
  ;;

  let pieces_of_colour_and_pt pos colour pt =
    BB.bb_and (pieces_of_colour pos colour) (pieces_of_pt pos pt)
  ;;

  let pieces_of_colour_and_pts pos colour pts =
    BB.bb_and (pieces_of_colour pos colour) (pieces_of_pts pos pts)
  ;;

  let count_by_piece { piece_count; _ } piece =
    Array.get piece_count @@ Types.piece_to_enum piece
  ;;

  let count_by_colour_and_pt pos colour pt =
    count_by_piece pos @@ Types.mk_piece colour pt
  ;;

  let count_by_pt pos pt =
    count_by_colour_and_pt pos Types.WHITE pt + count_by_colour_and_pt pos Types.BLACK pt
  ;;

  (* When there is only one of this piece on the board, get its square. *)
  let square_of_pt_and_colour pos pt colour =
    assert (count_by_colour_and_pt pos colour pt = 1);
    pieces_of_colour_and_pts pos colour [ pt ] |> BB.lsb |> BB.bb_to_square
  ;;

  let ep_square { st = { ep_square; _ }; _ } = ep_square

  (* TODO: Check what we are calling this with and where its used, maybe
     expecting enumbit*)
  (* See if any pieces are in the castling path *)
  let castling_impeded ({ castling_path; _ } as pos) cr =
    match cr with
    | Types.WHITE_OO | Types.WHITE_OOO | Types.BLACK_OO | Types.BLACK_OOO ->
      BB.bb_and
        (pieces pos)
        (Hashtbl.find castling_path cr |> Stdlib.Option.value ~default:BB.empty)
      |> BB.bb_not_zero
  ;;

  (* TODO: Check what we are calling this with and where its used, maybe
     expecting enumbit*)
  let castling_rook_square { castling_rook_square; _ } cr =
    match cr with
    | Types.WHITE_OO | Types.WHITE_OOO | Types.BLACK_OO | Types.BLACK_OOO ->
      Hashtbl.find castling_rook_square cr
  ;;

  (* Returns all the squares attacked by a certain colour *)
  let attacks_by pos pt colour =
    let rec helper threats attackers pt obstacles =
      if BB.bb_not_zero attackers
      then (
        let attacker, attackers = BB.pop_lsb attackers in
        let attacker_sq = BB.bb_to_square attacker in
        helper
          (BB.bb_or threats @@ BB.attacks_bb_occupied pt attacker_sq obstacles)
          attackers
          pt
          obstacles)
      else threats
    in
    match pt with
    | Types.PAWN ->
      BB.pawn_attacks_bb colour (pieces_of_colour_and_pt pos colour Types.PAWN)
    | _ -> helper BB.empty (pieces_of_colour_and_pt pos colour pt) pt (pieces pos)
  ;;

  let checkers { st = { checkers_bb; _ }; _ } = checkers_bb

  let blockers_for_king { st = { blockers_for_king; _ }; _ } colour =
    Array.get blockers_for_king @@ Types.colour_to_enum colour
  ;;

  let pinners { st = { pinners; _ }; _ } colour =
    Array.get pinners @@ Types.colour_to_enum colour
  ;;

  (* Get squares from which we would check the enemy king *)
  let check_squares { st = { check_squares; _ }; _ } pt =
    Array.get check_squares @@ Types.piece_type_to_enum pt
  ;;

  (* Based on a congruential pseudo-random number generator *)
  let make_key seed =
    UInt64.mul seed @@ UInt64.of_string "6364136223846793005"
    |> UInt64.add @@ UInt64.of_string "1442695040888963407"
  ;;

  (* TODO: How in the world does this work? *)
  let adjust_key50 { st = { rule50; _ }; _ } after_move k =
    let threshold = if after_move then 14 - 1 else 14 in
    if rule50 < threshold
    then k
    else UInt64.logor k @@ make_key (UInt64.of_int ((rule50 - threshold) / 8))
  ;;

  let key ({ st = { key; _ }; _ } as pos) = adjust_key50 pos false @@ key
  let pawn_key { st = { pawn_key; _ }; _ } = pawn_key
  let material_key { st = { material_key; _ }; _ } = material_key

  let non_pawn_material_for_colour { st = { non_pawn_material; _ }; _ } colour =
    Array.get non_pawn_material @@ Types.colour_to_enum colour
  ;;

  let non_pawn_material pos =
    non_pawn_material_for_colour pos Types.WHITE
    + non_pawn_material_for_colour pos Types.BLACK
  ;;

  let game_ply { game_ply; _ } = game_ply

  (* FIXME: Document what this means *)
  let rule50_count { st = { rule50; _ }; _ } = rule50
  let is_chess960 { chess960; _ } = chess960

  let can_castle { st = { castling_rights; _ }; _ } cr =
    Types.castling_right_to_enum cr land castling_rights <> 0
  ;;

  (* FIXME: I think this returns moves generated during the capture stage? *)
  let is_capture pos m =
    assert (Types.move_is_ok m);
    (* If the destination square is already occupied, then this is a capture. *)
    ((not (is_empty pos @@ Types.move_dst m))
     && not (Types.equal_move_type (Types.get_move_type m) Types.CASTLING))
    || Types.equal_move_type (Types.get_move_type m) Types.EN_PASSANT
  ;;

  (* Returns true if a move is generated from the capture stage, having also
     queen promotions covered, i.e. consistency with the capture stage move
     generation is needed to avoid the generation of duplicate moves. *)
  let is_capture_stage pos m =
    assert (Types.move_is_ok m);
    match is_capture pos m, Types.get_ppt m with
    | true, _ | _, Some Types.QUEEN -> true
    | _ -> false
  ;;

  let captured_piece { st = { captured_piece; _ }; _ } = captured_piece

  (* FIXME: Reconsider this mix of mutable and immutable data structure. Using
     functional updates alongside with inplace mutations doesn't feel very good. *)
  (* TODO: Consider caching total piece count and by_type_bb for all pieces *)
  let put_piece { board; by_type_bb; by_colour_bb; piece_count; _ } piece sq =
    let piece_type_enum = Types.piece_type_to_enum @@ Types.type_of_piece piece in
    let piece_enum = Types.piece_to_enum piece in
    let colour_enum = Types.colour_to_enum @@ Types.color_of_piece piece in
    Array.set board (Types.square_to_enum sq) (Some piece);
    Array.set
      by_type_bb
      piece_type_enum
      (BB.bb_or_sq (Array.get by_type_bb piece_type_enum) sq);
    Array.set
      by_colour_bb
      colour_enum
      (BB.bb_or_sq (Array.get by_colour_bb colour_enum) sq);
    Array.set piece_count piece_enum (Int.succ (Array.get piece_count piece_enum))
  ;;

  let remove_piece { board; by_type_bb; by_colour_bb; piece_count; _ } sq =
    match Array.get board (Types.square_to_enum sq) with
    | Some piece ->
      let piece_type_enum = Types.piece_type_to_enum @@ Types.type_of_piece piece in
      let piece_enum = Types.piece_to_enum piece in
      let colour_enum = Types.colour_to_enum @@ Types.color_of_piece piece in
      Array.set
        by_type_bb
        piece_type_enum
        (BB.bb_xor_sq (Array.get by_type_bb piece_type_enum) sq);
      Array.set
        by_colour_bb
        colour_enum
        (BB.bb_xor_sq (Array.get by_colour_bb colour_enum) sq);
      Array.set board (Types.square_to_enum sq) None;
      Array.set piece_count piece_enum (Int.pred (Array.get piece_count piece_enum))
    | None -> failwith "Removing nonexistent piece"
  ;;

  let move_piece { board; by_type_bb; by_colour_bb; _ } src dst =
    match Array.get board (Types.square_to_enum src) with
    | Some piece ->
      let src_dest = BB.square_bb src |> BB.sq_or_bb dst in
      let piece_type_enum = Types.piece_type_to_enum @@ Types.type_of_piece piece in
      let colour_enum = Types.colour_to_enum @@ Types.color_of_piece piece in
      Array.set
        by_type_bb
        piece_type_enum
        (BB.bb_xor (Array.get by_type_bb piece_type_enum) src_dest);
      Array.set
        by_colour_bb
        colour_enum
        (BB.bb_xor (Array.get by_colour_bb colour_enum) src_dest);
      Array.set board (Types.square_to_enum src) None;
      Array.set board (Types.square_to_enum dst) (Some piece)
    | None -> failwith "Moving nonexistent piece"
  ;;

  (*
     * Implements Marcel van Kervinck's cuckoo algorithm to detect repetition of positions
   * for 3-fold repetition draws. The algorithm uses two hash tables with Zobrist hashes
   * to allow fast detection of recurring positions. For details see:
   * http://web.archive.org/web/20201107002606/https://marcelk.net/2013-04-06/paper/upcoming-rep-v2.pdf
  *)

  type zobrist =
    { psq : key array array
    ; en_passant : key array
    ; castling : key array
    ; side : key
    ; no_pawns : key
    }

  (* First and second hash functions for indexing the cuckoo tables *)
  (*  h & 0x1fff *)
  let h1 h = UInt64.to_int @@ UInt64.logand h @@ UInt64.of_int 0x1fff

  (* (h >> 16) & 0x1fff *)
  let h2 h =
    UInt64.to_int @@ (UInt64.shift_right h 16 |> UInt64.logand @@ UInt64.of_int 0x1fff)
  ;;

  (* Cuckoo tables with Zobrist hashes of valid reversible moves, and the moves themselves *)
  let cuckoo = Array.create ~len:8192 UInt64.zero
  let cuckoo_move = Array.create ~len:8192 Types.none_move

  (* TODO: Test that this is right, but how? *)
  let zobrist_data =
    (* Helper function *)
    let rec insert_cuckoo key move i =
      let key' = Array.get cuckoo i in
      let move' = Array.get cuckoo_move i in
      Array.set cuckoo i key;
      Array.set cuckoo_move i move;
      if Types.equal_move move' Types.none_move
      then
        (* This is an empty slot so we are done *)
        ()
      else (
        (* Push victim to alternative slot *)
        let i = if i = h1 key then h2 key else h1 key in
        insert_cuckoo key' move' i)
    in
    Random.init 42;
    (* TODO: Find a better seed? *)
    let gen_uint64 () = Random.bits64 () |> UInt64.of_int64 in
    let psq =
      Array.make_matrix
        ~dimx:(List.length Types.all_pieces)
        ~dimy:(List.length Types.all_squares)
        UInt64.zero
    in
    let en_passant = Array.create ~len:(List.length Types.all_files) UInt64.zero in
    let castling = Array.create ~len:Types.castling_right_num_combinations UInt64.zero in
    let side = gen_uint64 () in
    let no_pawns = gen_uint64 () in
    List.iter
      (List.cartesian_product Types.all_pieces Types.all_squares)
      ~f:(fun (piece, sq) ->
        Utils.matrix_set
          psq
          (Types.piece_to_enum piece)
          (Types.square_to_enum sq)
          (gen_uint64 ()));
    List.iter Types.all_files ~f:(fun file ->
      Array.set en_passant (Types.file_to_enum file) (gen_uint64 ()));
    List.iter
      Utils.(0 -- (Types.castling_right_num_combinations - 1))
      ~f:(fun i -> Array.set castling i (gen_uint64 ()));
    (* Setup tables with all possible piece movements, we ignore pawn moves as they
       reset the repetition count *)
    let count = ref 0 in
    List.iter Types.all_pieces ~f:(fun piece ->
      ignore
      @@ List.fold
           ~init:(List.tl_exn Types.all_squares)
           ~f:(fun other_sqs s1 ->
             let s1_enum = Types.square_to_enum s1 in
             List.iter other_sqs ~f:(fun s2 ->
               let s2_enum = Types.square_to_enum s2 in
               let piece_type = Types.type_of_piece piece in
               if
                 (not (Types.equal_piece_type piece_type Types.PAWN))
                 && (BB.bb_not_zero
                     @@ (BB.attacks_bb_occupied piece_type s1 BB.empty |> BB.sq_and_bb s2)
                    )
               then (
                 let piece_enum = Types.piece_to_enum piece in
                 let m = Types.mk_move s2 s1 in
                 let key =
                   UInt64.Infix.(
                     Utils.matrix_get psq piece_enum s1_enum
                     lxor Utils.matrix_get psq piece_enum s2_enum
                     lxor side)
                 in
                 let i = h1 key in
                 insert_cuckoo key m i;
                 count := !count + 1));
             List.tl other_sqs |> Option.value ~default:[])
           Types.all_squares);
    assert (!count = 3668);
    { psq; en_passant; castling; side; no_pawns }
  ;;

  let get_zobrist_psq piece sq =
    Utils.matrix_get
      zobrist_data.psq
      (Types.piece_to_enum piece)
      (Types.square_to_enum sq)
  ;;

  let get_zobrist_psq_with_count piece count =
    Utils.matrix_get zobrist_data.psq (Types.piece_to_enum piece) count
  ;;

  let get_zobrist_ep sq =
    Array.get zobrist_data.en_passant (Types.file_of_sq sq |> Types.file_to_enum)
  ;;

  (* TODO: Test this function *)
  let set_castling_right
        ({ st; castling_rights_mask; castling_rook_square; castling_path; _ } as pos)
        colour
        rook_sq
    =
    let king_sq = square_of_pt_and_colour pos Types.KING colour in
    let cr =
      (* If king < rook, i.e. the king is on the left of the rook, that means that
         we are dealing with kingside castling. *)
      match colour, Types.compare_square king_sq rook_sq < 0 with
      | WHITE, true -> Types.WHITE_OO
      | WHITE, false -> Types.WHITE_OOO
      | BLACK, true -> Types.BLACK_OO
      | BLACK, false -> Types.BLACK_OOO
    in
    let cr_enum = Types.castling_right_to_enum cr in
    let castling_rights_mask =
      Map.update castling_rights_mask king_sq ~f:(fun v ->
        cr_enum lor Stdlib.Option.value v ~default:0)
    in
    let castling_rights_mask =
      Map.update castling_rights_mask rook_sq ~f:(fun v ->
        cr_enum lor Stdlib.Option.value v ~default:0)
    in
    let st = { st with castling_rights = st.castling_rights lor cr_enum } in
    ignore @@ Hashtbl.add castling_rook_square ~key:cr ~data:rook_sq;
    let king_dest_sq =
      Types.relative_sq colour
      @@ if Types.is_kingside_castling cr then Types.G1 else Types.C1
    in
    let rook_dest_sq =
      Types.relative_sq colour
      @@ if Types.is_kingside_castling cr then Types.F1 else Types.D1
    in
    (* The squares that the rook and king need to traverse in order to castle
       (this includes the destination squares of both pieces). We ignore the
       current squares of the king and rook because the pieces may 'cross' each
       other in chess960. *)
    let path =
      BB.Infix.(
        (BB.between_bb rook_sq rook_dest_sq || BB.between_bb king_sq king_dest_sq)
        & BB.bb_not (BB.square_bb king_sq || BB.square_bb rook_sq))
    in
    ignore @@ Hashtbl.add castling_path ~key:cr ~data:path;
    { pos with st; castling_rights_mask }
  ;;

  (*
     * Calculates st->blockersForKing[c] and st->pinners[~c],
   * which store respectively the pieces preventing king of color c from being
   * in check and the slider pieces of color ~c pinning pieces of color c to the
   * king.
  *)
  let update_slider_blockers ({ st = { blockers_for_king; pinners; _ }; _ } as pos) c =
    let other_c = Types.other_colour c in
    let king_sq = square_of_pt_and_colour pos Types.KING c in
    let our_pieces = pieces_of_colour pos c in
    (* Snipers are enemy sliders that attack `king_sq` when other snipers are
       not in the way. *)
    let snipers =
      BB.Infix.(
        ((BB.attacks_bb Types.ROOK king_sq & pieces_of_pts pos [ Types.QUEEN; Types.ROOK ])
         || (BB.attacks_bb Types.BISHOP king_sq
             & pieces_of_pts pos [ Types.QUEEN; Types.BISHOP ]))
        & pieces_of_colour pos other_c)
    in
    let occupancy = BB.bb_xor (pieces pos) snipers in
    let rec do_snipers snipers blockers pinners =
      if BB.bb_not_zero snipers
      then (
        let sniper_bb, others = BB.pop_lsb snipers in
        let sniper_sq = BB.bb_to_square sniper_bb in
        (* This will not include the sniper itself as it won't be in
           `occupancy` *)
        let candidate_blockers = BB.between_bb king_sq sniper_sq |> BB.bb_and occupancy in
        let blockers, pinners =
          (* Check if there is exactly one piece between the sniper and the
             king *)
          if
            BB.bb_not_zero candidate_blockers && not (BB.more_than_one candidate_blockers)
          then (
            let blockers = BB.bb_or blockers candidate_blockers in
            (* If the piece is ours, then the sniper is pinning it to our
               king*)
            if BB.bb_not_zero @@ BB.bb_and candidate_blockers our_pieces
            then blockers, BB.bb_or pinners sniper_bb
            else blockers, pinners)
          else blockers, pinners
        in
        do_snipers others blockers pinners)
      else blockers, pinners
    in
    let blockers_bb, pinners_bb = do_snipers snipers BB.empty BB.empty in
    Array.set blockers_for_king (Types.colour_to_enum c) blockers_bb;
    Array.set pinners (Types.colour_to_enum other_c) pinners_bb
  ;;

  (* Sets king attacks to detect if a move gives check *)
  let set_check_info ({ side_to_move; st; _ } as pos) =
    update_slider_blockers pos Types.WHITE;
    update_slider_blockers pos Types.BLACK;
    let enemy_colour = Types.other_colour side_to_move in
    (* Get square of opponent's king *)
    let king_sq = square_of_pt_and_colour pos Types.KING enemy_colour in
    let check_squares = st.check_squares in
    let set_check_squares pt = Array.set check_squares (Types.piece_type_to_enum pt) in
    let all_pieces = pieces pos in
    (* Populate the table with the squares from which the king may be attacked *)
    let bishop_attacks = BB.attacks_bb_occupied Types.BISHOP king_sq all_pieces in
    let rook_attacks = BB.attacks_bb_occupied Types.ROOK king_sq all_pieces in
    set_check_squares Types.PAWN (BB.pawn_attacks_bb_from_sq enemy_colour king_sq);
    set_check_squares
      Types.KNIGHT
      (BB.attacks_bb_occupied Types.KNIGHT king_sq all_pieces);
    set_check_squares Types.BISHOP bishop_attacks;
    set_check_squares Types.ROOK rook_attacks;
    set_check_squares Types.QUEEN (BB.bb_or bishop_attacks rook_attacks);
    set_check_squares Types.KING BB.empty
  ;;

  let attackers_to_occupied pos sq occupied =
    let pieces_of_colour_and_pt = pieces_of_colour_and_pt pos in
    let pieces_of_pt = pieces_of_pt pos in
    let pieces_of_pts = pieces_of_pts pos in
    BB.pawn_attacks_bb_from_sq Types.BLACK sq
    |> BB.bb_and (pieces_of_colour_and_pt Types.WHITE Types.PAWN)
    |> BB.bb_or
         (BB.pawn_attacks_bb_from_sq Types.WHITE sq
          |> BB.bb_and (pieces_of_colour_and_pt Types.BLACK Types.PAWN))
    |> BB.bb_or (BB.attacks_bb Types.KNIGHT sq |> BB.bb_and (pieces_of_pt Types.KNIGHT))
    |> BB.bb_or
         (BB.attacks_bb_occupied Types.ROOK sq occupied
          |> BB.bb_and (pieces_of_pts [ Types.ROOK; Types.QUEEN ]))
    |> BB.bb_or
         (BB.attacks_bb_occupied Types.BISHOP sq occupied
          |> BB.bb_and (pieces_of_pts [ Types.BISHOP; Types.QUEEN ]))
    |> BB.bb_or (BB.attacks_bb Types.KING sq |> BB.bb_and (pieces_of_pt Types.KING))
  ;;

  let attackers_to_sq pos sq = attackers_to_occupied pos sq (pieces pos)

  (* TODO: Unit test this *)
  let set_state ({ st; side_to_move; piece_count; _ } as pos) =
    let rec do_pieces pieces_bb (key, pawn_key, white_npm, black_npm) =
      if BB.bb_not_zero pieces_bb
      then (
        let sq_bb, rest = BB.pop_lsb pieces_bb in
        let sq = BB.bb_to_square sq_bb in
        let sq_enum = Types.square_to_enum sq in
        let piece = piece_on_exn pos sq in
        let piece_enum = Types.piece_to_enum piece in
        let key =
          UInt64.logxor key @@ Utils.matrix_get zobrist_data.psq piece_enum sq_enum
        in
        let pawn_key, white_npm, black_npm =
          match Types.type_of_piece piece with
          | Types.PAWN ->
            ( UInt64.logxor pawn_key
              @@ Utils.matrix_get zobrist_data.psq piece_enum sq_enum
            , white_npm
            , black_npm )
          (* King doesn't affect pawn key or npm *)
          | Types.KING -> pawn_key, white_npm, black_npm
          (* Increment npm *)
          | _ ->
            (match Types.color_of_piece piece with
             | Types.WHITE -> pawn_key, white_npm + Types.piece_value piece, black_npm
             | Types.BLACK -> pawn_key, white_npm, black_npm + Types.piece_value piece)
        in
        do_pieces rest (key, pawn_key, white_npm, black_npm))
      else key, pawn_key, white_npm, black_npm
    in
    let checkers_bb =
      attackers_to_sq pos (square_of_pt_and_colour pos Types.KING side_to_move)
      |> BB.bb_and (pieces_of_colour pos (Types.other_colour side_to_move))
    in
    set_check_info pos;
    let { castling_rights; ep_square; non_pawn_material; _ } = st in
    let all_pieces = pieces pos in
    let key, pawn_key, white_npm, black_npm =
      do_pieces
        all_pieces
        (UInt64.zero, zobrist_data.no_pawns, Types.value_zero, Types.value_zero)
    in
    Array.set non_pawn_material (Types.colour_to_enum Types.WHITE) white_npm;
    Array.set non_pawn_material (Types.colour_to_enum Types.BLACK) black_npm;
    let key =
      match ep_square with
      | Some ep_square ->
        UInt64.logxor key
        @@ Array.get zobrist_data.en_passant
        @@ Types.file_to_enum
        @@ Types.file_of_sq ep_square
      | None -> key
    in
    (* The default value is zobrist_data.side, we save computation by
       toggling it when its black to move. *)
    let key =
      match side_to_move with
      | Types.WHITE -> key
      | Types.BLACK -> UInt64.logxor key zobrist_data.side
    in
    let key = UInt64.logxor key @@ Array.get zobrist_data.castling castling_rights in
    let material_key =
      List.fold
        ~init:UInt64.zero
        ~f:(fun mk pc ->
          let piece_enum = Types.piece_to_enum pc in
          List.fold
            ~init:mk
            ~f:(fun mk cnt ->
              UInt64.logxor mk @@ Utils.matrix_get zobrist_data.psq piece_enum cnt)
            Utils.(0 -- (Array.get piece_count piece_enum - 1)))
        Types.all_pieces
    in
    { pos with st = { st with key; pawn_key; material_key; checkers_bb } }
  ;;

  (* TODO: Unit test this *)
  let legal ({ side_to_move = us; chess960; _ } as pos) m =
    assert (Types.move_is_ok m);
    let src = Types.move_src m in
    let dst = Types.move_dst m in
    let them = Types.other_colour us in
    (* Some sanity checks,
       1. Moved piece is our colour
       2. There is exactly one king of our colour on the board *)
    assert (Types.equal_colour us @@ Types.color_of_piece @@ moved_piece_exn pos m);
    assert (
      Types.equal_piece
        (piece_on_exn pos @@ square_of_pt_and_colour pos Types.KING us)
        (Types.mk_piece us Types.KING));
    if Types.equal_move_type (Types.get_move_type m) Types.EN_PASSANT
    then (
      (* TODO: Check if move generation would still generate this move if
         we are in check, if that's the case, fix the below description! *)
      (* En passant would be legal as long as the king is not under attack after
         the move, i.e. this also handles the case where the king is already in
         check as capturing en passant never blocks checks. *)
      let king_sq = square_of_pt_and_colour pos Types.KING us in
      (* The captured pawn is one square behind our destination *)
      let captured_sq =
        Types.sq_sub_dir dst (Types.pawn_push_direction us) |> Stdlib.Option.get
      in
      let occupied =
        pieces pos |> BB.sq_xor_bb src |> BB.sq_xor_bb captured_sq |> BB.sq_or_bb dst
      in
      assert (Types.equal_square dst (Stdlib.Option.get @@ ep_square pos));
      assert (Types.equal_piece (moved_piece_exn pos m) (Types.mk_piece us Types.PAWN));
      assert (
        Types.equal_piece (piece_on_exn pos captured_sq) (Types.mk_piece them Types.PAWN));
      assert (piece_on pos dst |> Option.is_none);
      (* Test that the king is not in ray of an enemy slider. *)
      BB.bb_is_empty
        (BB.attacks_bb_occupied Types.ROOK king_sq occupied
         |> BB.bb_and (pieces_of_colour_and_pts pos them [ Types.QUEEN; Types.ROOK ]))
      && BB.bb_is_empty
           (BB.attacks_bb_occupied Types.BISHOP king_sq occupied
            |> BB.bb_and (pieces_of_colour_and_pts pos them [ Types.QUEEN; Types.BISHOP ])
           ))
    else if Types.equal_move_type (Types.get_move_type m) Types.CASTLING
    then (
      (* Castling moves generation does not check if the castling path is clear of
         enemy attacks, it relies on this function to verify that. *)
      (* After castling, the rook and king final positions are the same in
         Chess960 as they would be in standard chess. *)
      let dst =
        (* We store the position of the rook as the destination *)
        Types.relative_sq
          us
          (if Types.compare_square dst src > 0 then Types.G1 else Types.C1)
      in
      (* Get a direction that we will use to trace our steps to see if the
         king was ever exposed. *)
      let step = if Types.compare_square dst src > 0 then Types.WEST else Types.EAST in
      let rec is_path_safe curr_sq =
        (* Check all squares in the path between src (exclusive) and
           dst (inclusive) *)
        if Types.equal_square curr_sq src
        then true
        else if
          attackers_to_sq pos curr_sq
          |> BB.bb_and @@ pieces_of_colour pos them
          |> BB.bb_not_zero
        then false
        else is_path_safe (Types.sq_plus_dir curr_sq step |> Stdlib.Option.get)
      in
      (* In chess960, the rook could be shielding the king from some sideway
         attack, it which case we would not be able to castle, e.g.
         an enemy queen on A1 when the rook is on B1. *)
      is_path_safe dst
      && ((not chess960)
          || BB.bb_is_empty (blockers_for_king pos us |> BB.sq_and_bb @@ Types.move_dst m)
         ))
    else if
      Types.equal_piece_type Types.KING @@ Types.type_of_piece @@ piece_on_exn pos src
    then
      (* If the moving piece is a king, check whether the destination square is
         attacked by the opponent. *)
      attackers_to_occupied pos dst (pieces pos |> BB.sq_xor_bb src)
      |> BB.bb_and @@ pieces_of_colour pos them
      |> BB.bb_is_empty
    else
      (* We have a non-king move, it would be legal as long as the piece is
         not pinned. If it is, then it needs to be moving along the ray between
         the king and the pinner. *)
      BB.bb_and_sq (blockers_for_king pos us) src |> BB.bb_is_empty
      || BB.aligned src dst (square_of_pt_and_colour pos Types.KING us)
  ;;

  (* Takes a random move and tests whether the move is
     pseudo-legal. It is used to validate moves from TT that can be corrupted
     due to SMP concurrent access or hash position key aliasing. *)
  (* TODO: This huge function must be unit tested *)
  let pseudo_legal ({ side_to_move = us; _ } as pos) m =
    let them = Types.other_colour us in
    let src = Types.move_src m in
    let dst = Types.move_dst m in
    let our_pieces = pieces_of_colour pos them in
    let their_pieces = pieces_of_colour pos them in
    let all_pieces = pieces pos in
    (* TODO: Figure out if null-moves may be passed to this fn. *)
    let piece = moved_piece pos m in
    (* Use a slower but simpler function for uncommon cases
       yet we skip the legality check of MoveList<LEGAL>(). *)
    if not @@ Types.equal_move_type (Types.get_move_type m) Types.NORMAL
    then failwith "TODO: Implement this after move generation is done"
    else (
      (* FIXME: We know this is not a promotion, hence `ppt` should be None,
         but we only have 4 bits to represent the `ppt`, so we put the
         responsibility on the caller to not rely on the `ppt` when the
         move type is not PROMOTION *)
      (* assert (Types.get_ppt m |> Option.is_none); *)
      match piece with
      | None -> false (* If there is no piece, the move is definitely illegal *)
      | Some piece ->
        let piece_type = Types.type_of_piece piece in
        (* If the moved piece doesn't belong to the side to move *)
        if not @@ Types.equal_colour us @@ Types.color_of_piece piece
        then false (* If the destination square is occupied by a friendly piece *)
        else if BB.bb_not_zero (BB.bb_and_sq our_pieces dst)
        then false
        else if
          (* Handle pawn moves:
               1. This can't be a promotion, hence the destination cannot be
               on the 1st and last ranks *)
          Types.equal_piece_type piece_type Types.PAWN
          && BB.bb_not_zero (BB.bb_or BB.rank_8 BB.rank_1 |> BB.sq_and_bb dst)
        then false
        else if
          (* 2. It must be either a capture, a single push, or double push *)
          Types.equal_piece_type piece_type Types.PAWN
          (* Not a capture *)
          && BB.bb_is_empty
               (BB.pawn_attacks_bb_from_sq us src
                |> BB.bb_and their_pieces
                |> BB.sq_and_bb dst)
          (* Not a single push *)
          && (not
                (Types.equal_square
                   (Types.sq_plus_dir src (Types.pawn_push_direction us)
                    |> Stdlib.Option.get)
                   dst
                 && is_empty pos dst))
          (* Not a double push *)
          && not
               (Types.equal_square
                  (Types.sq_plus_dir_twice src (Types.pawn_push_direction us)
                   |> Stdlib.Option.get)
                  dst
                (* Verify that pawn is on starting rank *)
                && (Types.equal_rank Types.RANK_2 @@ Types.relative_rank_of_sq us src)
                (* Verify that both squares in front of the pawn are empty *)
                && is_empty pos dst
                && is_empty
                     pos
                     (Types.sq_sub_dir dst (Types.pawn_push_direction us)
                      |> Stdlib.Option.get))
        then false
        else if
          (* If its not a pawn, then the destination square must be a
               square that it attacks. *)
          (not @@ Types.equal_piece_type piece_type Types.PAWN)
          && BB.bb_is_empty
               (BB.attacks_bb_occupied piece_type src all_pieces |> BB.sq_and_bb dst)
        then
          false
          (* Evasions generator already takes care to avoid some kind of
               illegal moves and legal() relies on this. We therefore have
               to take care that the same kind of moves are filtered out
               here. *)
        else if BB.bb_not_zero @@ checkers pos
        then (
          let checkers' = checkers pos in
          if not @@ Types.equal_piece_type Types.KING piece_type
          then
            (* There is more than one checker, hence moving the king
                 cannot be legal. *)
            if BB.more_than_one checkers'
            then false
            else
              (* Since we are not moving the king, we must either capture
                   the attacker or interpose the check. *)
              BB.bb_not_zero
                (BB.between_bb
                   (square_of_pt_and_colour pos Types.KING us)
                   (BB.lsb checkers' |> BB.bb_to_square)
                 |> BB.sq_and_bb dst)
          else
            (* Ensure that we are not moving the king to another attacked
                 square. *)
            BB.bb_is_empty
              (attackers_to_occupied pos dst (BB.bb_xor_sq all_pieces src)
               |> BB.bb_and their_pieces))
        else true)
  ;;

  (* Tests whether a pseudo-legal move gives a check *)
  (* TODO: Unit test this *)
  let gives_check ({ side_to_move = us; _ } as pos) m =
    assert (Types.move_is_ok m);
    match moved_piece pos m with
    | None -> failwith "Move source contains no piece"
    | Some piece ->
      let pt = Types.type_of_piece piece in
      let src = Types.move_src m in
      let dst = Types.move_dst m in
      let them = Types.other_colour us in
      let their_king_sq = square_of_pt_and_colour pos Types.KING them in
      assert (Types.equal_colour us @@ Types.color_of_piece piece);
      (* Direct check? i.e. the piece moves to a square where it
           checks the enemy king. *)
      if BB.bb_not_zero (check_squares pos pt |> BB.sq_and_bb dst)
      then
        true
        (* Discovered check? If the enemy king's blocker moves away from
             the ray so it no longer shields the king. *)
      else if BB.bb_not_zero (blockers_for_king pos them |> BB.sq_and_bb src)
      then
        (* If the piece is no longer aligned with the enemy king OR
             the moved piece is a KING and he is castling away. Now, for
             the king to be the shield of a discovered check, it must mean
             that the enemy king is on the same rank as the other king, and
             is shielding a check from the rook, hence, castling would
             definitely deliver a check. *)
        (not @@ BB.aligned src dst their_king_sq)
        || (Types.equal_move_type Types.CASTLING @@ Types.get_move_type m)
      else (
        match Types.get_move_type m with
        (* We already checked all possible ways for a normal move to deliver
             check *)
        | Types.NORMAL -> false
        | Types.PROMOTION ->
          (* Will a newly promoted piece directly attack the king? *)
          BB.bb_not_zero
            (BB.attacks_bb_occupied
               (Stdlib.Option.get @@ Types.get_ppt m)
               dst
               (pieces pos |> BB.sq_xor_bb src)
             |> BB.sq_and_bb their_king_sq)
        | Types.EN_PASSANT ->
          (* We have already handled the normal checks, here we just need
                 to see if the removal of the captured pawn delivers a discovered
                 check. *)
          let cap_sq =
            Types.mk_square ~file:(Types.file_of_sq dst) ~rank:(Types.rank_of_sq src)
          in
          let b =
            pieces pos |> BB.sq_xor_bb src |> BB.sq_xor_bb cap_sq |> BB.sq_or_bb dst
          in
          BB.bb_or
            (BB.attacks_bb_occupied Types.ROOK their_king_sq b
             |> BB.bb_and (pieces_of_colour_and_pts pos us [ Types.QUEEN; Types.ROOK ]))
            (BB.attacks_bb_occupied Types.BISHOP their_king_sq b
             |> BB.bb_and (pieces_of_colour_and_pts pos us [ Types.QUEEN; Types.BISHOP ])
            )
          |> BB.bb_not_zero
        | Types.CASTLING ->
          (* We have the KING on the src sq and the ROOK on the dst sq,
                 we just need to determine if a ROOK on its new square would
                 be a check. *)
          let rook_dst =
            (* Essentially checking if the ROOK is on the right of the
                   KING *)
            Types.relative_sq
              us
              (if Types.compare_square dst src > 0 then Types.F1 else Types.D1)
          in
          check_squares pos Types.ROOK |> BB.sq_and_bb rook_dst |> BB.bb_not_zero)
  ;;

  (* Performs some consistency checks for the position object
     and raise an assert if something wrong is detected.
     This is meant to be helpful when debugging. *)
  let pos_is_ok ({ side_to_move; piece_count; board; castling_rights_mask; _ } as pos) =
    let fast = true in
    let them = Types.other_colour side_to_move in
    (* Check that kings are where they should be *)
    assert (
      Types.equal_piece Types.W_KING
      @@ piece_on_exn pos (square_of_pt_and_colour pos Types.KING Types.WHITE));
    assert (
      Types.equal_piece Types.B_KING
      @@ piece_on_exn pos (square_of_pt_and_colour pos Types.KING Types.BLACK));
    (* Check that *)
    (match ep_square pos with
     | Some sq ->
       assert (Types.equal_rank Types.RANK_6 @@ Types.relative_rank_of_sq side_to_move sq)
     | None -> ());
    if fast
    then true
    else (
      (* Check king count *)
      assert (Array.get piece_count (Types.piece_to_enum Types.W_KING) = 1);
      assert (Array.get piece_count (Types.piece_to_enum Types.B_KING) = 1);
      (* It musn't be the case that the king of the opposing player is under
         attack on our turn. *)
      assert (
        attackers_to_sq pos (square_of_pt_and_colour pos Types.KING them)
        |> BB.bb_and (pieces_of_colour pos side_to_move)
        |> BB.bb_is_empty);
      (* No pawns on the first and eight rank *)
      assert (
        pieces_of_pt pos Types.PAWN
        |> BB.bb_and (BB.bb_or BB.rank_1 BB.rank_8)
        |> BB.bb_is_empty);
      assert (Array.get piece_count (Types.piece_to_enum Types.W_PAWN) <= 8);
      assert (Array.get piece_count (Types.piece_to_enum Types.B_PAWN) <= 8);
      (* Check bitboards*)

      (* Pieces of both colours do not overlap *)
      assert (
        pieces_of_colour pos Types.WHITE
        |> BB.bb_and (pieces_of_colour pos Types.BLACK)
        |> BB.bb_is_empty);
      (* white | black == all pieces *)
      assert (
        BB.equal
          (pieces_of_colour pos Types.WHITE |> BB.bb_or (pieces_of_colour pos Types.BLACK))
          (pieces pos));
      assert (BB.popcount (pieces_of_colour pos Types.WHITE) <= 16);
      assert (BB.popcount (pieces_of_colour pos Types.BLACK) <= 16);
      (* Different piece types do not overlap *)
      List.iter
        (List.cartesian_product Types.all_piece_types Types.all_piece_types)
        ~f:(fun (pt1, pt2) ->
          if not @@ Types.equal_piece_type pt1 pt2
          then
            assert (
              pieces_of_pt pos pt1 |> BB.bb_and (pieces_of_pt pos pt2) |> BB.bb_is_empty));
      (* Check piece counts *)
      List.iter Types.all_pieces ~f:(fun pc ->
        (* Check that piece counts are tracking the bitboards *)
        assert (
          count_by_piece pos pc
          = BB.popcount
            @@ pieces_of_colour_and_pt
                 pos
                 (Types.color_of_piece pc)
                 (Types.type_of_piece pc));
        (* Check that piece counts are tracking what's on the board *)
        assert (
          Array.count board ~f:(fun pc' ->
            match pc' with
            | Some pc' -> Types.equal_piece pc pc'
            | None -> false)
          = count_by_piece pos pc));
      (* Check castling rights *)
      List.iter
        [ Types.WHITE, Types.WHITE_OO
        ; Types.WHITE, Types.WHITE_OOO
        ; Types.BLACK, Types.BLACK_OO
        ; Types.BLACK, Types.BLACK_OOO
        ]
        ~f:(fun (colour, cr) ->
          if can_castle pos cr
          then (
            let cr_enum = Types.castling_right_to_enum cr in
            let crsq = castling_rook_square pos cr |> Stdlib.Option.get in
            (* Check that castling rook square is right *)
            assert (
              Types.equal_piece (Types.mk_piece colour Types.ROOK) (piece_on_exn pos crsq));
            (* Check that the rook has the right castling rights mask *)
            assert (Map.find_exn castling_rights_mask crsq = cr_enum);
            (* Check that the king also has the right mask *)
            assert (
              Map.find_exn
                castling_rights_mask
                (square_of_pt_and_colour pos Types.KING colour)
              land cr_enum
              = cr_enum)));
      true)
  ;;

  (* `src` and `dst` are what are encoded into a castling move, i.e.
     `src` is the KING square and `dst` is the ROOK's square.
     `is_do` controls whether the functions does or undoes the castling *)
  let do_castling pos is_do us src dst =
    (* If the rook is on the right of the king, we are castling kingside. *)
    let is_kingside = Types.compare_square dst src > 0 in
    (* This is based on how we encoded castling *)
    let rook_src = dst in
    let rook_dst = Types.relative_sq us (if is_kingside then Types.F1 else Types.D1) in
    let king_src = src in
    let king_dst = Types.relative_sq us (if is_kingside then Types.G1 else Types.C1) in
    (* TODO: Some NNUE stuff here *)
    (* Remove both pieces first since squares could overlap in Chess960 *)
    remove_piece pos @@ if is_do then king_src else king_dst;
    remove_piece pos @@ if is_do then rook_src else rook_dst;
    put_piece pos (Types.mk_piece us Types.KING) (if is_do then king_dst else king_src);
    put_piece pos (Types.mk_piece us Types.ROOK) (if is_do then rook_dst else rook_src);
    king_dst, rook_src, rook_dst
  ;;

  let new_st () =
    { material_key = UInt64.zero
    ; pawn_key = UInt64.zero
    ; castling_rights = 0
    ; rule50 = 0
    ; plies_from_null = 0
    ; ep_square = None
    ; previous = None
    ; non_pawn_material = Array.create ~len:2 0
    ; key = UInt64.zero
    ; checkers_bb = BB.empty
    ; blockers_for_king = Array.create ~len:2 BB.empty
    ; pinners = Array.create ~len:2 BB.empty
    ; check_squares = Array.create ~len:(List.length Types.all_piece_types) BB.empty
    ; captured_piece = None
    ; repetition = 0
    }
  ;;

  let new_st_from_prev
        ({ (* This needs to be duplicated *)
           non_pawn_material
         ; _
         } as st)
    =
    { st with
      previous = Some st
    ; non_pawn_material = Array.copy non_pawn_material
    ; (* These need to reinitialised *)
      key = UInt64.zero
    ; checkers_bb = BB.empty
    ; blockers_for_king = Array.create ~len:2 BB.empty
    ; pinners = Array.create ~len:2 BB.empty
    ; check_squares = Array.create ~len:(List.length Types.all_piece_types) BB.empty
    ; captured_piece = None
    ; repetition = 0
    }
  ;;

  (* Creates a full copy of `st` for null moves *)
  let new_full_copy_st_from_prev
        ({ (* These fields need to be duplicated *)
           non_pawn_material
         ; blockers_for_king
         ; pinners
         ; check_squares
         ; _
         } as st)
    =
    (* Copy all the arrays *)
    { st with
      previous = Some st
    ; non_pawn_material = Array.copy non_pawn_material
    ; blockers_for_king = Array.copy blockers_for_king
    ; pinners = Array.copy pinners
    ; check_squares = Array.copy check_squares
    }
  ;;

  (* Three possible values,
     - 0 : No repetition
     - >0 : Distance from the previous occurrence of this position
     - <0 : Distance from the previous occurence and is a 3-fold *)
  let get_repetition_info ({ rule50; plies_from_null; key; _ } as st) =
    (* Take two steps back along the chain *)
    let get_prev st = Stdlib.Option.get @@ (Stdlib.Option.get st.previous).previous in
    let end_idx = Int.min rule50 plies_from_null in
    let rec helper i stp =
      if i <= end_idx
      then (
        let stp = get_prev stp in
        if UInt64.equal key stp.key
        then
          (* If that position has already repeated, then this will
             be a 3-fold, hence we return a negative value *)
          if stp.repetition <> 0 then -i else i
        else helper (i + 2) stp)
      else (* Not enough moves to find a repetition *)
        0
    in
    (* We start at 4, as it is impossible to get a 3-fold repetition
       earlier than this. *)
    if end_idx >= 4 then helper 4 (get_prev st) else 0
  ;;

  (* Makes a move, and saves all information necessary
     to a StateInfo object. The move is assumed to be legal. Pseudo-legal
     moves should be filtered out before this function is called. *)
  (* TODO: Unit test this *)
  let do_move
        ({ st; game_ply; side_to_move = us; piece_count; castling_rights_mask; _ } as pos)
        m
        gives_check
    =
    assert (Types.move_is_ok m);
    let key = UInt64.logxor st.key zobrist_data.side in
    let old_st = st in
    (* Increment ply counters. In particular, rule50 will be reset to zero later on
       in case of a capture or a pawn move. *)
    let new_st =
      { (new_st_from_prev st) with
        rule50 = old_st.rule50 + 1
      ; plies_from_null = old_st.plies_from_null + 1
      }
    in
    let game_ply = game_ply + 1 in
    (* TODO: There is NNUE stuff to handle here *)
    let them = Types.other_colour us in
    let src = Types.move_src m in
    let dst = Types.move_dst m in
    let piece = piece_on_exn pos src in
    let captured_piece =
      match Types.get_move_type m with
      | Types.EN_PASSANT -> Some (Types.mk_piece them Types.PAWN)
      | _ -> piece_on pos dst
    in
    let is_castling = Types.equal_move_type Types.CASTLING @@ Types.get_move_type m in
    (* Sanity checks *)
    assert (Types.equal_colour us @@ Types.color_of_piece piece);
    (match captured_piece with
     | Some piece ->
       (* We encode castling as capturing our own rook *)
       assert (
         Types.equal_colour
           (Types.color_of_piece piece)
           (if is_castling then us else them));
       assert (not @@ Types.equal_piece_type Types.KING (Types.type_of_piece piece))
     | None -> ());
    (* Handle castling, this also clears out the captured piece, setting
       the stage for the below. *)
    let captured_piece, dst, key =
      if is_castling
      then (
        let our_rook = Types.mk_piece us Types.ROOK in
        assert (Types.equal_piece piece @@ Types.mk_piece us Types.KING);
        assert (Types.equal_piece (Stdlib.Option.get captured_piece) our_rook);
        let king_dst, rook_src, rook_dst = do_castling pos true us src dst in
        let key =
          UInt64.logxor key (get_zobrist_psq our_rook rook_src)
          |> UInt64.logxor (get_zobrist_psq our_rook rook_dst)
        in
        None, king_dst, key)
      else captured_piece, dst, key
    in
    (* Handle piece captures *)
    let key, new_st =
      match captured_piece with
      | Some captured_piece ->
        let capture_sq = dst in
        (* If it was an en passant, the captured sq maybe something else *)
        let capture_sq, new_st =
          match Types.type_of_piece captured_piece with
          | Types.PAWN ->
            let capture_sq =
              match Types.get_move_type m with
              | Types.EN_PASSANT ->
                let res =
                  Types.sq_sub_dir capture_sq @@ Types.pawn_push_direction us
                  |> Stdlib.Option.get
                in
                (* Some sanity checks *)
                assert (Types.equal_piece piece (Types.mk_piece us Types.PAWN));
                assert (Types.equal_square dst (Stdlib.Option.get old_st.ep_square));
                assert (Types.equal_rank (Types.relative_rank_of_sq us dst) Types.RANK_6);
                assert (Option.is_none @@ piece_on pos dst);
                assert (
                  Types.equal_piece
                    (piece_on_exn pos capture_sq)
                    (Types.mk_piece them Types.PAWN));
                res
              | _ -> capture_sq
            in
            ( capture_sq
            , { new_st with
                pawn_key =
                  UInt64.logxor new_st.pawn_key
                  @@ get_zobrist_psq captured_piece capture_sq
              } )
          | _ ->
            let piece_val = Types.piece_value captured_piece in
            Array.set
              new_st.non_pawn_material
              (Types.colour_to_enum them)
              (Array.get new_st.non_pawn_material (Types.colour_to_enum them) - piece_val);
            capture_sq, new_st
        in
        (* TODO: NNUE stuff *)
        remove_piece pos capture_sq;
        (* Update material hash key and prefetch access to materialTable *)
        let key = UInt64.logxor key @@ get_zobrist_psq captured_piece capture_sq in
        ( key
        , { new_st with
            (* Reset rule50 *)
            rule50 = 0
          ; (* FIXME: What is this really doing? *)
            material_key =
              UInt64.logxor new_st.material_key
              @@ Utils.matrix_get zobrist_data.psq (Types.piece_to_enum captured_piece)
              @@ Array.get piece_count (Types.piece_to_enum captured_piece)
          } )
      | None -> key, new_st
    in
    (* Update hash key *)
    let key =
      UInt64.logxor key (get_zobrist_psq piece src)
      |> UInt64.logxor (get_zobrist_psq piece dst)
    in
    (* Reset en passant square *)
    let key, new_st =
      match new_st.ep_square with
      | Some sq ->
        UInt64.logxor key @@ get_zobrist_ep sq, { new_st with ep_square = None }
      | None -> key, new_st
    in
    let src_dst_cr_mask =
      (Map.find castling_rights_mask src |> Option.value ~default:0)
      lor (Map.find castling_rights_mask dst |> Option.value ~default:0)
    in
    (* Update castling rights if needed *)
    let key, new_st =
      if new_st.castling_rights <> 0 && src_dst_cr_mask <> 0
      then (
        (* Remove old castling rights from the key *)
        let key =
          UInt64.logxor key (Array.get zobrist_data.castling new_st.castling_rights)
        in
        (* Amend castling rights *)
        let castling_rights = new_st.castling_rights land lnot src_dst_cr_mask in
        (* Add it to the key *)
        let key = UInt64.logxor key (Array.get zobrist_data.castling castling_rights) in
        key, { new_st with castling_rights })
      else key, new_st
    in
    (* For castling, this move would have been handled earlier *)
    (* TODO: NNUE stuff *)
    if not is_castling then move_piece pos src dst;
    (* Some extra work needs to be done for pawn moves *)
    let key, new_st =
      if Types.equal_piece_type Types.PAWN (Types.type_of_piece piece)
      then (
        (* Maybe set en passant square *)
        let new_st, key =
          let candidate_ep_sq =
            Types.sq_sub_dir dst (Types.pawn_push_direction us) |> Stdlib.Option.get
          in
          if
            (* FIXME: This doesn't seem to be a nice way to check that a pawn
               advanced by 2 squares *)
            Types.square_to_enum src lxor Types.square_to_enum dst = 16
            && (BB.bb_not_zero
                @@ BB.bb_and
                     (BB.pawn_attacks_bb_from_sq us candidate_ep_sq)
                     (pieces_of_colour_and_pt pos them Types.PAWN))
          then
            ( { new_st with ep_square = Some candidate_ep_sq }
            , UInt64.logxor key @@ get_zobrist_ep candidate_ep_sq )
          else { new_st with ep_square = None }, key
        in
        (* Handle promotion *)
        let key, new_st =
          if Types.equal_move_type Types.PROMOTION (Types.get_move_type m)
          then (
            let promote_to = Types.mk_piece us (Types.get_ppt m |> Stdlib.Option.get) in
            assert (Types.equal_rank Types.RANK_8 (Types.relative_rank_of_sq us dst));
            (* Remove the pawn and put the new piece there *)
            remove_piece pos dst;
            put_piece pos promote_to dst;
            (* TODO: NNUE stuff *)

            (* Update hash keys *)
            let key =
              UInt64.logxor key (get_zobrist_psq piece dst)
              |> UInt64.logxor (get_zobrist_psq promote_to dst)
            in
            Array.set
              new_st.non_pawn_material
              (Types.colour_to_enum us)
              (Array.get new_st.non_pawn_material (Types.colour_to_enum us)
               + Types.piece_value promote_to);
            ( key
            , { new_st with
                pawn_key = UInt64.logxor new_st.pawn_key (get_zobrist_psq piece dst)
              ; (* TODO: Why -1? *)
                material_key =
                  UInt64.logxor new_st.material_key
                  @@ get_zobrist_psq_with_count
                       promote_to
                       (Array.get piece_count (Types.piece_to_enum promote_to) - 1)
                  |> UInt64.logxor
                     @@ get_zobrist_psq_with_count
                          piece
                          (Array.get piece_count (Types.piece_to_enum piece))
              } ))
          else key, new_st
        in
        ( key
        , { new_st with
            (* Pawn hash key *)
            pawn_key =
              UInt64.logxor new_st.pawn_key (get_zobrist_psq piece src)
              |> UInt64.logxor (get_zobrist_psq piece dst)
          ; (* Pawn move resets 50-move rule *)
            rule50 = 0
          } ))
      else key, new_st
    in
    (* TODO: Check that its fine to call attackers_to with old st, AFAIK
       it does not touch `st` so it's fine since `pos` contains updated data. *)
    let checkers_bb =
      if gives_check
      then
        attackers_to_sq pos (square_of_pt_and_colour pos Types.KING them)
        |> BB.bb_and (pieces_of_colour pos us)
      else BB.empty
    in
    let new_st =
      { new_st with
        checkers_bb
      ; key
      ; captured_piece
      ; repetition = get_repetition_info new_st
      }
    in
    let new_pos = { pos with st = new_st; side_to_move = them; game_ply } in
    (* We need to construct new `pos` before setting check info as it will
       expect to see the fresh state. *)
    (* Update king attacks used for fast check detection *)
    set_check_info new_pos;
    assert (pos_is_ok pos);
    new_pos
  ;;

  (* Convenience function that does a move while also checking if it
     gives check *)
  let do_move' pos move = do_move pos move (gives_check pos move)

  (* Undos a move. When it returns, the position should
     be restored to exactly the same state as before the move was made. *)
  (* TODO: Unit test this *)
  let undo_move
        ({ side_to_move; game_ply; st = { captured_piece; previous; _ }; _ } as pos)
        m
    =
    assert (Types.move_is_ok m);
    let us = Types.other_colour side_to_move in
    let them = side_to_move in
    let src = Types.move_src m in
    let dst = Types.move_dst m in
    let pc = piece_on_exn pos dst in
    let pos = { pos with side_to_move = us } in
    (* The source should be empty now... unless it was CASTLING, in which
       case the square could be occupied in chess960. *)
    assert (
      is_empty pos src || (Types.equal_move_type Types.CASTLING @@ Types.get_move_type m));
    (match captured_piece with
     | Some pc ->
       assert (not @@ Types.equal_piece_type Types.KING @@ Types.type_of_piece pc)
     | None -> ());
    if Types.equal_move_type Types.PROMOTION @@ Types.get_move_type m
    then (
      let pc = piece_on_exn pos dst in
      assert (Types.equal_rank Types.RANK_8 (Types.relative_rank_of_sq us dst));
      assert (
        Types.equal_piece_type
          (Types.type_of_piece pc)
          (Types.get_ppt m |> Stdlib.Option.get));
      (* Swap the piece with a pawn as the remaining code will handle it *)
      remove_piece pos dst;
      put_piece pos (Types.mk_piece us Types.PAWN) dst);
    (match Types.get_move_type m with
     | Types.CASTLING -> ignore @@ do_castling pos false us src dst
     | _ ->
       (* Move piece back to where it was originally *)
       move_piece pos dst src;
       (match captured_piece with
        | Some captured_piece ->
          let cap_sq =
            (* Captured sq is not the destination if it was en passant *)
            if Types.equal_move_type Types.EN_PASSANT @@ Types.get_move_type m
            then (
              let res =
                Types.sq_sub_dir dst (Types.pawn_push_direction us) |> Stdlib.Option.get
              in
              (* Check that it was indeed a pawn move *)
              assert (Types.equal_piece_type Types.PAWN @@ Types.type_of_piece pc);
              (* Check that the pawn was on the previous ep_square on the
                   6th rank*)
              assert (
                Types.equal_square dst
                @@ Stdlib.Option.get
                @@ (Stdlib.Option.get previous).ep_square);
              assert (Types.equal_rank Types.RANK_6 @@ Types.relative_rank_of_sq us dst);
              (* Check that the supposedly captured pawn is not on its
                   original square *)
              assert (Option.is_none @@ piece_on pos res);
              (* Check that the captured piece was an enemy pawn *)
              assert (Types.equal_piece captured_piece @@ Types.mk_piece them Types.PAWN);
              res)
            else dst
          in
          (* Restore the captured piece *)
          put_piece pos captured_piece cap_sq
        | None -> ()));
    let pos = { pos with game_ply = game_ply - 1; st = Stdlib.Option.get previous } in
    assert (pos_is_ok pos);
    pos
  ;;

  let do_null_move ({ st; _ } as pos) =
    (* We can't do a null move when the player is in check *)
    assert (BB.bb_is_empty @@ checkers pos);
    let new_st = new_full_copy_st_from_prev st in
    (* TODO: NNUE stuff *)
    (* Remove en passant square and update key *)
    let new_st =
      match new_st.ep_square with
      | Some ep_square ->
        { new_st with
          key = UInt64.logxor new_st.key @@ get_zobrist_ep ep_square
        ; ep_square = None
        }
      | None -> new_st
    in
    let new_st =
      { new_st with
        key = UInt64.logxor new_st.key zobrist_data.side
      ; rule50 = new_st.rule50 + 1
      }
    in
    (* TODO: Prefetch TT entry *)
    let new_st = { new_st with plies_from_null = 0; repetition = 0 } in
    (* Build a new pos with the updated info so we can set check_info *)
    let pos =
      { pos with st = new_st; side_to_move = Types.other_colour pos.side_to_move }
    in
    set_check_info pos;
    assert (pos_is_ok pos);
    pos
  ;;

  (* The only way to undo a null move *)
  let undo_null_move ({ st; side_to_move; _ } as pos) =
    (* No one should be checked before/after applying a null move *)
    assert (BB.bb_is_empty @@ checkers pos);
    { pos with
      side_to_move = Types.other_colour side_to_move
    ; st = Stdlib.Option.get st.previous
    }
  ;;

  (* TODO: Could this be a null move? *)
  (* FIXME: Figure out what this is really used for and document it better *)
  (*
   * Computes the new hash key after the given move. Needed for speculative
   * prefetch. It doesn't recognize special moves like castling, en passant
   * and promotions.
   *)

  let key_after ({ st = { key; _ }; _ } as pos) m =
    let src = Types.move_src m in
    let dst = Types.move_dst m in
    let piece = piece_on_exn pos src in
    let captured = piece_on pos src in
    let k = UInt64.logxor key zobrist_data.side in
    let k =
      match captured with
      | Some captured -> get_zobrist_psq captured dst
      | None -> k
    in
    let k =
      UInt64.logxor k (get_zobrist_psq piece src)
      |> UInt64.logxor (get_zobrist_psq piece dst)
    in
    if
      Option.is_some captured
      || (Types.equal_piece_type Types.PAWN @@ Types.type_of_piece piece)
    then k
    else adjust_key50 pos true k
  ;;

  (*
   * Tests if the SEE (Static Exchange Evaluation)
   * value of move is greater or equal to the given threshold. We'll use an
   * algorithm similar to alpha-beta pruning with a null window.
   *)
  (* TODO: Unit test this *)
  let see_ge ({ side_to_move; _ } as pos) m threshold =
    let src = Types.move_src m in
    let dst = Types.move_dst m in
    let rec helper stm attackers occupied swap res =
      (* FIXME: Changing the colour here is weird, I should change it
         during the recursive call maybe? *)
      let stm = Types.other_colour stm in
      let them = Types.other_colour stm in
      (* Occupied contains all the pieces on the board, we make sure that
         the attackers contain only pieces that still exist. *)
      let attackers = BB.bb_and attackers occupied in
      let stm_attackers = BB.bb_and attackers (pieces_of_colour pos stm) in
      (* If stm has no more attackers then give up: stm loses *)
      if BB.bb_is_empty stm_attackers
      then res
      else (
        (* Don't allow pinned pieces to attack as long as there are
           pinners on their original square. *)
        let stm_attackers =
          if BB.bb_not_zero (pinners pos them |> BB.bb_and occupied)
          then BB.bb_and stm_attackers (BB.bb_not @@ blockers_for_king pos stm)
          else stm_attackers
        in
        (* Check again if there are any attackers left *)
        (* TODO: Can I combine the two checks? Try this after adding unit
           tests *)
        if BB.bb_is_empty stm_attackers
        then res
        else (
          (* Flip the result since the stm still has attackers, in the
             recursive call if the other side has no more pieces, then this
             shall be the final result. *)
          let res = res lxor 1 in
          (* FIXME: HMM maybe it would be nice to refactor these so that we don't
             need to generate all the attackers in one go since we might not
             require all of them. *)
          let pawn_attackers = BB.bb_and stm_attackers (pieces_of_pt pos Types.PAWN) in
          let knight_attackers =
            BB.bb_and stm_attackers (pieces_of_pt pos Types.KNIGHT)
          in
          let bishop_attackers =
            BB.bb_and stm_attackers (pieces_of_pt pos Types.BISHOP)
          in
          let rook_attackers = BB.bb_and stm_attackers (pieces_of_pt pos Types.ROOK) in
          let queen_attackers = BB.bb_and stm_attackers (pieces_of_pt pos Types.QUEEN) in
          (* Locate and remove the next least valuable attacker, and add to
             the bitboard 'attackers' any X-ray attackers behind it. *)
          if BB.bb_not_zero pawn_attackers
          then (
            let swap = Types.pawn_value - swap in
            (* If giving up the least valuable piece isn't good enough, then
               nothing will be. The same logic applies in the other branches
               too. *)
            if swap < res
            then res
            else (
              let occupied = BB.bb_xor occupied (BB.lsb pawn_attackers) in
              (* Since pawns attack like bishops, them moving could
                 unleash other attackers that attack diagonally. *)
              let attackers =
                BB.bb_or
                  attackers
                  (BB.attacks_bb_occupied Types.BISHOP dst occupied
                   |> BB.bb_and @@ pieces_of_pts pos [ Types.QUEEN; Types.BISHOP ])
              in
              helper stm attackers occupied swap res))
          else if BB.bb_not_zero knight_attackers
          then (
            let swap = Types.knight_value - swap in
            if swap < res
            then res
            else (
              let occupied = BB.bb_xor occupied (BB.lsb knight_attackers) in
              (* By moving a knight, this will not unleash more attackers
                 on the destination square. *)
              helper stm attackers occupied swap res))
          else if BB.bb_not_zero bishop_attackers
          then (
            let swap = Types.bishop_value - swap in
            if swap < res
            then res
            else (
              let occupied = BB.bb_xor occupied (BB.lsb bishop_attackers) in
              let attackers =
                BB.bb_or
                  attackers
                  (BB.attacks_bb_occupied Types.BISHOP dst occupied
                   |> BB.bb_and @@ pieces_of_pts pos [ Types.QUEEN; Types.BISHOP ])
              in
              helper stm attackers occupied swap res))
          else if BB.bb_not_zero rook_attackers
          then (
            let swap = Types.rook_value - swap in
            if swap < res
            then res
            else (
              let occupied = BB.bb_xor occupied (BB.lsb rook_attackers) in
              let attackers =
                BB.bb_or
                  attackers
                  (BB.attacks_bb_occupied Types.ROOK dst occupied
                   |> BB.bb_and @@ pieces_of_pts pos [ Types.QUEEN; Types.ROOK ])
              in
              helper stm attackers occupied swap res))
          else if BB.bb_not_zero queen_attackers
          then (
            let swap = Types.queen_value - swap in
            if swap < res
            then res
            else (
              let occupied = BB.bb_xor occupied (BB.lsb queen_attackers) in
              let attackers =
                BB.bb_or
                  attackers
                  (BB.attacks_bb_occupied Types.ROOK dst occupied
                   |> BB.bb_and @@ pieces_of_pts pos [ Types.QUEEN; Types.ROOK ])
                |> BB.bb_or
                     (BB.attacks_bb_occupied Types.BISHOP dst occupied
                      |> BB.bb_and @@ pieces_of_pts pos [ Types.QUEEN; Types.BISHOP ])
              in
              helper stm attackers occupied swap res))
          else if
            (* KING:
               If we "capture" with the king but the opponent still has attackers,
               reverse the result. *)
            BB.bb_not_zero (BB.bb_and attackers @@ BB.bb_not (pieces_of_colour pos stm))
          then res lxor 1
          else res))
    in
    assert (Types.move_is_ok m);
    (* Only deal with normal moves, assume others pass a simple SEE, i.e.
       we do a test with 0 as the value *)
    if not @@ Types.equal_move_type Types.NORMAL @@ Types.get_move_type m
    then Types.value_zero >= threshold
    else (
      (* Value of winning the destination piece *)
      let dst_piece_value =
        match piece_on pos dst with
        | Some p -> Types.piece_value p
        | None -> 0
      in
      let swap = dst_piece_value - threshold in
      (* If taking it for free isn't good enough, then nothing will be *)
      if swap < 0
      then false
      else (
        let swap = (Types.piece_value @@ piece_on_exn pos src) - swap in
        (* If trading our piece for the target is good enough, then we
           don't have to look any further (since we won't be obliged to capture
           back if this piece gets taken) *)
        if swap <= 0
        then true
        else (
          assert (
            Types.equal_colour side_to_move
            @@ Types.color_of_piece
            @@ piece_on_exn pos src);
          (* Occupancy after exchanging once
             xoring `dst` is important for pinned piece logic *)
          let occupied = pieces pos |> BB.sq_xor_bb src |> BB.sq_xor_bb dst in
          let attackers = attackers_to_occupied pos dst occupied in
          (* Convert the int to bool *)
          helper side_to_move attackers occupied swap 1 <> 0)))
  ;;

  (*
   * Tests whether the position is drawn by 50-move rule
   * or by repetition. It does not detect stalemates.
   *)
  (* TODO: Document what the argument `ply` here means *)
  let is_draw ({ st = { rule50; repetition; _ }; _ } as pos) ply =
    (* 50-move rule *)
    (* TODO: Check that move list is not empty*)
    (rule50 > 99 && (BB.bb_is_empty @@ checkers pos))
    ||
    (* Return a draw score if a position repeats once earlier but strictly
          after the root, or repeats twice before or at the root. *)
    (* Note that negative values represent 3-fold repetitions, and 0 means
          that there was no repetition at all *)
    (repetition <> 0 && repetition < ply)
  ;;

  (* Tests whether there has been at least one repetition
     of positions since the last capture or pawn move. *)
  let has_repeated { st = { rule50; plies_from_null; _ } as st; _ } =
    let end_idx = Int.min rule50 plies_from_null in
    let rec helper { repetition; previous; _ } idx =
      if idx >= 4
      then if repetition <> 0 then true else helper (Stdlib.Option.get previous) (idx - 1)
      else false
    in
    helper st end_idx
  ;;

  (* Tests if the position has a move which draws by repetition,
     or an earlier position has a move that directly reaches the current position. *)
  (* TODO: Document the argument `ply` *)
  let has_game_cycle
        ({ st = { rule50; plies_from_null; key = original_key; previous; _ }
         ; side_to_move
         ; _
         } as pos)
        ply
    =
    let end_idx = Int.min rule50 plies_from_null in
    if end_idx < 3
    then false
    else (
      let all_pieces = pieces pos in
      let rec loop i stp =
        if i <= end_idx
        then (
          let stp = Stdlib.Option.get @@ (Stdlib.Option.get stp.previous).previous in
          (* The key of the move that would bring us from stp's position
             to the current position *)
          let move_key = UInt64.logxor original_key stp.key in
          (* To see if the above move is a valid reversible move, look for it
             in the cuckoo table. See if either H1 or H2 gives us the right hash. *)
          let cuckoo_idx =
            if UInt64.equal move_key (Array.get cuckoo @@ h1 move_key)
            then Some (h1 move_key)
            else if UInt64.equal move_key (Array.get cuckoo @@ h2 move_key)
            then Some (h2 move_key)
            else None
          in
          match cuckoo_idx with
          | Some j ->
            let move = Array.get cuckoo_move j in
            let s1 = Types.move_src move in
            let s2 = Types.move_dst move in
            (* Check that the move is actually possible, i.e there are no obstacles
                 between s1 and s2.
                 NOTE: `between_bb` includes the second sq, that's why we xor `s2` *)
            if
              BB.bb_is_empty
                (BB.between_bb s1 s2 |> BB.sq_xor_bb s2 |> BB.bb_and all_pieces)
            then
              (* Did this move occur after the root? *)
              if ply > i
              then true
              else if
                (* For nodes before or at the root, check that the move is a
                     repetition rather than a move to the current position.
                     In the cuckoo table, both moves Rc1c5 and Rc5c1 are stored
                     in the same location, so we have to select which square to
                     check.
                     For repetitions before or at the root, require one more
                     repetition by checking if the move was already repeated
                     previously. *)
                (Types.equal_colour side_to_move
                 @@ Types.color_of_piece
                      (piece_on_exn pos (if is_empty pos s1 then s2 else s1)))
                && stp.repetition <> 0
              then true
              else loop (i + 2) stp
            else failwith "TODO"
          | None -> loop (i + 2) stp)
        else false
      in
      loop 3 (Stdlib.Option.get previous))
  ;;

  (* TODO: Implement `flip` after we implement `fen` function *)
  (* TODO: Implement this *)
  let psq_score _pos = Score.mk 0 0

  (* Make empty position *)
  let mk () =
    { board = Array.create ~len:64 None
    ; by_type_bb = Array.create ~len:(List.length Types.all_piece_types) BB.empty
    ; by_colour_bb = Array.create ~len:2 BB.empty
    ; piece_count = Array.create ~len:(List.length Types.all_pieces) 0
    ; castling_rights_mask = Map.empty (module SquareCmp)
    ; (* TODO: Reduce the repetition? *)
      castling_rook_square =
        Hashtbl.create
          ~size:(List.length Types.all_castling_rights)
          ~growth_allowed:false
          (module struct
            type t = Types.castling_right [@@deriving sexp, hash]

            let compare = Types.compare_castling_right
          end)
    ; castling_path =
        Hashtbl.create
          ~size:(List.length Types.all_castling_rights)
          ~growth_allowed:false
          (module struct
            type t = Types.castling_right [@@deriving sexp, hash]

            let compare = Types.compare_castling_right
          end)
    ; game_ply = 0
    ; side_to_move = Types.WHITE
    ; chess960 = false
    ; st = new_st ()
    }
  ;;

  (*
     *    A FEN string defines a particular position using only the ASCII character set.
   * 
   *    A FEN string contains six fields separated by a space. The fields are:
   * 
   *    1) Piece placement (from white's perspective). Each rank is described, starting
   *       with rank 8 and ending with rank 1. Within each rank, the contents of each
   *       square are described from file A through file H. Following the Standard
   *       Algebraic Notation (SAN), each piece is identified by a single letter taken
   *       from the standard English names. White pieces are designated using upper-case
   *       letters ("PNBRQK") whilst Black uses lowercase ("pnbrqk"). Blank squares are
   *       noted using digits 1 through 8 (the number of blank squares), and "/"
   *       separates ranks.
   * 
   *    2) Active color. "w" means white moves next, "b" means black.
   * 
   *    3) Castling availability. If neither side can castle, this is "-". Otherwise,
   *       this has one or more letters: "K" (White can castle kingside), "Q" (White
   *       can castle queenside), "k" (Black can castle kingside), and/or "q" (Black
   *       can castle queenside).
   * 
   *    4) En passant target square (in algebraic notation). If there's no en passant
   *       target square, this is "-". If a pawn has just made a 2-square move, this
   *       is the position "behind" the pawn. Following X-FEN standard, this is recorded
   *       only if there is a pawn in position to make an en passant capture, and if
   *       there really is a pawn that might have advanced two squares.
   * 
   *    5) Halfmove clock. This is the number of halfmoves since the last pawn advance
   *       or capture. This is used to determine if a draw can be claimed under the
   *       fifty-move rule.
   * 
   *    6) Fullmove number. The number of the full move. It starts at 1, and is
   *       incremented after Black's move.
   *
  *)
  let from_fen ?(chess960 = false) fen_str =
    (* Helpers *)
    let char_to_piece = function
      | 'P' -> Some Types.W_PAWN
      | 'N' -> Some Types.W_KNIGHT
      | 'B' -> Some Types.W_BISHOP
      | 'R' -> Some Types.W_ROOK
      | 'Q' -> Some Types.W_QUEEN
      | 'K' -> Some Types.W_KING
      | 'p' -> Some Types.B_PAWN
      | 'n' -> Some Types.B_KNIGHT
      | 'b' -> Some Types.B_BISHOP
      | 'r' -> Some Types.B_ROOK
      | 'q' -> Some Types.B_QUEEN
      | 'k' -> Some Types.B_KING
      | _ -> None
    in
    let pos = mk () in
    let fen_str = String.to_list @@ String.lstrip fen_str in
    let bottom_squares = Types.sqs_of_rank RANK_1 in
    (* 1. Get piece placement *)
    let pos, fen_str =
      (* init with pos * rank * file *)
      Utils.fold_until
        fen_str
        ~init:(pos, 7, 0)
        ~finish:(fun (pos, _, _) -> pos)
        ~f:(fun (pos, rank, file) ch ->
          if Char.equal ch ' '
          then Utils.Stop pos
          else if Utils.Char.is_digit ch
          then Utils.Continue (pos, rank, file + Utils.Char.char_to_int_exn ch)
          else if Char.equal ch '/'
          then Utils.Continue (pos, rank - 1, 0)
          else (
            match char_to_piece ch with
            | Some piece ->
              let sq =
                Types.mk_square
                  ~file:(Types.file_of_enum file |> Stdlib.Option.get)
                  ~rank:(Types.rank_of_enum rank |> Stdlib.Option.get)
              in
              put_piece pos piece sq;
              Utils.Continue (pos, rank, file + 1)
              (* TODO: This should return an error? *)
            | None -> Utils.Continue (pos, rank, file + 1)))
    in
    let fen_str = String.of_list fen_str |> String.strip in
    let regex =
      Str.regexp
        {|\(w\|b\) *\([K\|Q\|k\|q\|a-h\|A-H]*\) *\([a-z][0-9]\|-\) *\([0-9]*\) *\([0-9]*\)|}
    in
    if Str.string_match regex fen_str 0
    then (
      let colour_string = Str.matched_group 1 fen_str in
      let castling_string = Str.matched_group 2 fen_str in
      let ep_square_string = Str.matched_group 3 fen_str in
      let fifty_move_string = Str.matched_group 4 fen_str in
      let half_moves_string = Str.matched_group 5 fen_str in
      (* 2. Active colour *)
      let side_to_move =
        if String.equal colour_string "w" then Types.WHITE else Types.BLACK
      in
      (* 3. Castling availability. Compatible with 3 standards: Normal FEN standard,
         Shredder-FEN that uses the letters of the columns on which the rooks began
         the game instead of KQkq and also X-FEN standard that, in case of Chess960,
         if an inner rook is associated with the castling right, the castling tag is
         replaced by the file letter of the involved rook, as for the Shredder-FEN. *)
      let pos =
        List.fold
          (String.to_list castling_string)
          ~init:{ pos with side_to_move }
          ~f:(fun pos ch ->
            let colour = if Utils.Char.is_lower ch then Types.BLACK else Types.WHITE in
            let rook = Types.mk_piece colour Types.ROOK in
            let rsq =
              match Stdlib.Char.uppercase_ascii ch with
              | 'K' ->
                (* Go right to left *)
                List.fold_until
                  (List.rev bottom_squares)
                  ~init:None
                  ~finish:(fun _ -> None)
                  ~f:(fun acc sq ->
                    let rsq = Types.relative_sq colour sq in
                    match piece_on pos rsq with
                    | Some piece when Types.equal_piece piece rook -> Stop (Some rsq)
                    | _ -> Continue acc)
              | 'Q' ->
                (* Go left to right *)
                List.fold_until
                  bottom_squares
                  ~init:None
                  ~finish:(fun _ -> None)
                  ~f:(fun acc sq ->
                    let rsq = Types.relative_sq colour sq in
                    match piece_on pos rsq with
                    | Some piece when Types.equal_piece piece rook -> Stop (Some rsq)
                    | _ -> Continue acc)
              | 'A' .. 'H' as ch ->
                Some
                  (Types.mk_square
                     ~file:
                       (Stdlib.Option.get
                        @@ Types.file_of_enum (Stdlib.Char.code ch - Stdlib.Char.code 'A')
                       )
                     ~rank:(Types.relative_rank colour Types.RANK_1))
              | _ -> None
            in
            match rsq with
            | Some rsq -> set_castling_right pos colour rsq
            | None -> pos)
      in
      let pos =
        (* 4. En passant square.
           Ignore if square is invalid or not on side to move relative rank 6. *)
        match String.to_list ep_square_string with
        | [ '-' ] -> pos
        | [ col; row ] ->
          (match col, row, pos.side_to_move with
           | 'a' .. 'h', '6', WHITE | 'a' .. 'h', '3', BLACK ->
             let ep_square =
               Types.mk_square
                 ~file:
                   (Stdlib.Char.code col - Stdlib.Char.code 'a'
                    |> Types.file_of_enum
                    |> Stdlib.Option.get)
                 ~rank:
                   (Stdlib.Char.code row - Stdlib.Char.code '1'
                    |> Types.rank_of_enum
                    |> Stdlib.Option.get)
             in
             (* En passant square will be considered only if
                   a) side to move have a pawn threatening epSquare
                   b) there is an enemy pawn in front of epSquare
                   c) there is no piece on epSquare or behind epSquare *)
             let other_side_to_move = Types.other_colour pos.side_to_move in
             let attackers =
               BB.bb_and
                 (BB.pawn_attacks_bb other_side_to_move (BB.square_bb ep_square))
                 (pieces_of_colour_and_pt pos pos.side_to_move PAWN)
             in
             let ep_square_front =
               BB.bb_and
                 (pieces_of_colour_and_pt pos other_side_to_move PAWN)
                 (Types.sq_plus_dir
                    ep_square
                    (Types.pawn_push_direction other_side_to_move)
                  |> Stdlib.Option.get
                  |> BB.square_bb)
             in
             let ep_square_on_behind =
               BB.bb_and
                 (pieces pos)
                 (Types.sq_plus_dir ep_square (Types.pawn_push_direction pos.side_to_move)
                  |> Stdlib.Option.get
                  |> BB.square_bb
                  |> BB.sq_or_bb ep_square)
             in
             let pos =
               if
                 BB.bb_not_zero attackers
                 && BB.bb_not_zero ep_square_front
                 && BB.bb_is_empty ep_square_on_behind
               then { pos with st = { pos.st with ep_square = Some ep_square } }
               else pos
             in
             pos
           | _ -> pos)
        (* This shouldn't be possible since we have a regex match *)
        | _ -> assert false
      in
      (* 5-6. Halfmove clock and fullmove number *)
      let rule50 =
        Int.of_string_opt fifty_move_string |> Stdlib.Option.value ~default:0
      in
      let game_ply =
        let tmp = Int.of_string_opt half_moves_string |> Stdlib.Option.value ~default:0 in
        Int.max (2 * (tmp - 1)) 0
        + Bool.to_int (Types.equal_colour pos.side_to_move BLACK)
      in
      let pos = set_state { pos with game_ply; chess960; st = { pos.st with rule50 } } in
      assert (pos_is_ok pos);
      Ok pos)
    else Error "Invalid FEN format"
  ;;

  let is_in_check pos = BB.bb_not_zero @@ checkers pos

  (* Produces a copy of the position *)
  (* TODO: remove the dynamically allocated stuff so I don't need to do this *)
  let copy pos =
    { pos with
      board = Array.copy pos.board
    ; by_type_bb = Array.copy pos.by_type_bb
    ; by_colour_bb = Array.copy pos.by_colour_bb
    ; piece_count = Array.copy pos.piece_count
    ; st =
        { pos.st with
          non_pawn_material = Array.copy pos.st.non_pawn_material
        ; blockers_for_king = Array.copy pos.st.blockers_for_king
        ; pinners = Array.copy pos.st.pinners
        ; check_squares = Array.copy pos.st.check_squares
        }
    }
  ;;
end

let%test_unit "dummy_test" = ()

(* TODO: Might want to add more tests, just to be sure. *)
let%test_unit "load_start_position_fen" =
  let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" in
  match Position.from_fen ~chess960:false fen with
  | Ok pos -> assert (Position.pos_is_ok pos)
  | Error _ -> assert false
;;
