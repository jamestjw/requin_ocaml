open Bitboard
open Base
open Position
open Types
module P = Position
module BB = Bitboard

let grain_size = 8

(* Bonus for having the side to move (modified by Joona Kiiski) *)
let tempo = Score.mk 24 11

type eval_info =
  { (* attackedBy[color][piece type] is a bitboard representing all squares
     attacked by a given color and piece type, attackedBy[color][0] contains
     all squares attacked by the given color. *)
    attacked_by : BB.t array array
  ; (* kingRing[color] is the zone around the king which is considered
     by the king safety evaluation. This consists of the squares directly
     adjacent to the king, and the three (or two, for a king on an edge file)
     squares two ranks in front of the king. For instance, if black's king
     is on g8, kingRing[BLACK] is a bitboard containing the squares f8, h8,
     f7, g7, h7, f6, g6 and h6. *)
    king_ring : BB.t * BB.t
  ; (* kingAttackersCount[color] is the number of pieces of the given color
     which attack a square in the kingRing of the enemy king. *)
    king_attackers_count : int * int
  ; (* kingAttackersWeight[color] is the sum of the "weight" of the pieces of the
     given color which attack a square in the kingRing of the enemy king. The
     weights of the individual piece types are given by the variables
     QueenAttackWeight, RookAttackWeight, BishopAttackWeight and
     KnightAttackWeight in `evaluate.ml` *)
    king_attackers_weight : int * int
  ; (* kingAdjacentZoneAttacksCount[color] is the number of attacks to squares
     directly adjacent to the king of the given color. Pieces which attack
     more than one square are counted multiple times. For instance, if black's
     king is on g8 and there's a white knight on g5, this knight adds
     2 to kingAdjacentZoneAttacksCount[BLACK]. *)
    king_adjacent_zone_attacks_count : int * int
  }

let mk_eval_info () =
  { attacked_by =
      Array.make_matrix ~dimx:2 ~dimy:(List.length Types.all_piece_types) BB.empty
  ; king_ring = BB.empty, BB.empty
  ; king_attackers_count = 0, 0
  ; king_adjacent_zone_attacks_count = 0, 0
  ; king_attackers_weight = 0, 0
  }
;;

let get_attacked_by { attacked_by; _ } colour piece_type =
  Utils.matrix_get
    attacked_by
    (Types.colour_to_enum colour)
    (Types.piece_type_to_enum piece_type)
;;

let set_attacked_by ({ attacked_by; _ } as ei) colour piece_type value =
  Utils.matrix_set
    attacked_by
    (Types.colour_to_enum colour)
    (Types.piece_type_to_enum piece_type)
    value;
  ei
;;

let get_king_ring color { king_ring = w, b; _ } =
  match color with
  | Types.WHITE -> w
  | Types.BLACK -> b
;;

let set_king_ring color value ({ king_ring = w, b; _ } as ei) =
  match color with
  | Types.WHITE -> { ei with king_ring = value, b }
  | Types.BLACK -> { ei with king_ring = w, value }
;;

let set_king_attackers_count color value ({ king_attackers_count = w, b; _ } as ei) =
  match color with
  | Types.WHITE -> { ei with king_attackers_count = value, b }
  | Types.BLACK -> { ei with king_attackers_count = w, value }
;;

let set_king_attackers_weight color value ({ king_attackers_weight = w, b; _ } as ei) =
  match color with
  | Types.WHITE -> { ei with king_attackers_weight = value, b }
  | Types.BLACK -> { ei with king_attackers_weight = w, value }
;;

let set_king_adjacent_zone_attacks_count
      color
      value
      ({ king_adjacent_zone_attacks_count = w, b; _ } as ei)
  =
  match color with
  | Types.WHITE -> { ei with king_adjacent_zone_attacks_count = value, b }
  | Types.BLACK -> { ei with king_adjacent_zone_attacks_count = w, value }
;;

let init_eval_info pos us ei =
  let them = Types.other_colour us in
  let squares_around_their_king =
    BB.attacks_bb Types.KING (P.square_of_pt_and_colour pos Types.KING them)
  in
  let our_pawn_attacks =
    BB.pawn_attacks_bb us (P.pieces_of_colour_and_pt pos us Types.PAWN)
  in
  Utils.matrix_set
    ei.attacked_by
    (Types.colour_to_enum them)
    (Types.piece_type_to_enum Types.KING)
    squares_around_their_king;
  Utils.matrix_set
    ei.attacked_by
    (Types.colour_to_enum us)
    (Types.piece_type_to_enum Types.PAWN)
    our_pawn_attacks;
  (* Init king safety tables only if we are going to use them *)
  if
    P.count_by_colour_and_pt pos us Types.QUEEN > 0
    && P.non_pawn_material_for_colour pos us
       >= Types.piece_type_value Types.QUEEN + Types.piece_type_value Types.ROOK
  then (
    let their_king_ring =
      BB.bb_or
        squares_around_their_king
        (BB.shift
           squares_around_their_king
           (match us with
            | Types.WHITE -> Types.SOUTH
            | Types.BLACK -> Types.NORTH))
    in
    let b = BB.bb_and squares_around_their_king our_pawn_attacks in
    let our_king_attackers_count = if BB.bb_not_zero b then BB.popcount b else 0 in
    set_king_ring them their_king_ring ei
    |> set_king_attackers_count us our_king_attackers_count
    |> set_king_adjacent_zone_attacks_count us 0
    |> set_king_attackers_weight us 0)
  else set_king_ring them BB.empty ei |> set_king_attackers_count us 0
;;

let mobility_bonus =
  let data =
    [ (* Pawns *)
      []
    ; [ (* Knights *)
        -38, -33
      ; -25, -23
      ; -12, -13
      ; 0, -3
      ; 12, 7
      ; 25, 17
      ; 31, 22
      ; 38, 27
      ; 38, 27
      ]
    ; [ (* Bishops *)
        -25, -30
      ; -11, -16
      ; 3, -2
      ; 17, 12
      ; 31, 26
      ; 45, 40
      ; 57, 52
      ; 65, 60
      ; 71, 65
      ; 74, 69
      ; 76, 71
      ; 78, 73
      ; 79, 74
      ; 80, 75
      ; 81, 76
      ; 81, 76
      ]
    ; [ (* Rooks *)
        -20, -36
      ; -14, -19
      ; -8, -3
      ; -2, 13
      ; 4, 29
      ; 10, 46
      ; 14, 62
      ; 19, 79
      ; 23, 95
      ; 26, 106
      ; 27, 111
      ; 28, 114
      ; 29, 116
      ; 30, 117
      ; 31, 118
      ; 32, 118
      ]
    ; [ (* Queens *)
        -10, -18
      ; -8, -13
      ; -6, -7
      ; -3, -2
      ; -1, 3
      ; 1, 8
      ; 3, 13
      ; 5, 19
      ; 8, 23
      ; 10, 27
      ; 12, 32
      ; 15, 34
      ; 16, 35
      ; 17, 35
      ; 18, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ; 20, 35
      ]
    ]
  in
  List.map ~f:(fun l -> Array.of_list @@ List.map ~f:(Utils.uncurry Score.mk) l) data
  |> Array.of_list
;;

(*
   * Returns a static, purely materialistic evaluation of the position from
 * the point of view of the given color. It can be divided by PawnValue to get
 * an approximation of the material advantage on the board in terms of pawns.
*)
let simple_eval pos color =
  let them = Types.other_colour color in
  (Types.pawn_value
   * (P.count_by_colour_and_pt pos color Types.PAWN
      - P.count_by_colour_and_pt pos them Types.PAWN))
  + (P.non_pawn_material_for_colour pos color - P.non_pawn_material_for_colour pos them)
;;

(*
   * interpolate() interpolates between a middle game and an endgame score,
 * based on game phase. It also scales the return value by a ScaleFactor array.
*)

let interpolate
      ({ mg; eg } : Score.t)
      (phase : Material.phase)
      (sf : Material.scale_factor)
  =
  assert (mg > -Types.value_infinite && mg < Types.value_infinite);
  assert (eg > -Types.value_infinite && eg < Types.value_infinite);
  assert (phase >= Material.phase_endgame && phase <= Material.phase_midgame);
  let ev = eg * sf / Material.scale_factor_normal in
  let result = ((mg * phase) + (ev * (128 - phase))) / 128 in
  (result + (grain_size / 2)) land lnot (grain_size - 1)
;;

let king_attack_weights = function
  | Types.PAWN -> 0
  | Types.BISHOP | Types.KNIGHT -> 2
  | Types.ROOK -> 3
  | Types.QUEEN -> 5
  | _ -> failwith "Invalid pt for this function"
;;

(* ThreatenedByPawnPenalty[PieceType] contains a penalty according to which
   piece type is attacked by an enemy pawn. *)
let threatened_by_pawn_penalty =
  let data = [ 0, 0; 56, 70; 56, 70; 76, 99; 86, 118 ] in
  List.map ~f:(Utils.uncurry Score.mk) data |> Array.of_list
;;

let evaluate_pieces_of_color pos ei us piece_type mobility_area =
  let them = Types.other_colour us in
  let piece_bb = P.pieces_of_colour_and_pt pos us piece_type in
  let _their_king_square = P.square_of_pt_and_colour pos Types.KING them in
  let do_sq
        ( attacked_by
        , king_attackers_count
        , king_attackers_weight
        , king_adjacent_zone_attacks_count
        , mobility
        , score )
        sq
    =
    (* Find attacked squares, including x-ray attacks for bishops and rooks *)
    let b =
      match piece_type with
      | Types.KNIGHT | Types.QUEEN -> BB.attacks_bb_occupied piece_type sq @@ P.pieces pos
      | Types.BISHOP ->
        BB.attacks_bb_occupied Types.BISHOP sq
        @@ BB.bb_xor (P.pieces pos) (P.pieces_of_colour_and_pt pos us Types.QUEEN)
      | Types.ROOK ->
        BB.attacks_bb_occupied Types.ROOK sq
        @@ BB.bb_xor
             (P.pieces pos)
             (P.pieces_of_colour_and_pts pos us [ Types.ROOK; Types.QUEEN ])
      | _ -> assert false
    in
    let attacked_by = BB.bb_or attacked_by b in
    let king_attackers_count, king_attackers_weight, king_adjacent_zone_attacks_count =
      if BB.bb_not_zero (BB.bb_and b (get_king_ring them ei))
      then (
        let bb =
          BB.bb_and b
          @@ Utils.matrix_get
               ei.attacked_by
               (Types.colour_to_enum them)
               (Types.piece_type_to_enum Types.KING)
        in
        let kazac_delta = if BB.bb_not_zero bb then BB.popcount bb else 0 in
        ( king_attackers_count + 1
        , king_attackers_weight + king_attack_weights piece_type
        , king_adjacent_zone_attacks_count + kazac_delta ))
      else king_attackers_count, king_attackers_weight, king_adjacent_zone_attacks_count
    in
    let mob = BB.popcount @@ BB.bb_and b mobility_area in
    let mobility =
      Score.Infix.(
        mobility
        + Utils.matrix_get mobility_bonus (Types.piece_type_to_enum piece_type) mob)
    in
    (* TODO: Add a bonus if a slider is pinning an enemy piece *)
    (* let score =
       match piece_type with
       | (Types.BISHOP | Types.ROOK | Types.QUEEN)
         when BB.bb_not_zero
                (BB.attacks_bb piece_type their_king_square |> BB.sq_and_bb sq)
         ->
         (* FIXME: Need to remove destination sq from between_bb *)
           let b =
             BB.between_bb sq their_king_square |> BB.bb_and (P.pieces pos)
           in
           assert (BB.bb_not_zero b);
           if
             (not (BB.more_than_one b))
             && (BB.bb_not_zero @@ BB.bb_and b (P.pieces_of_colour pos them))
           then failwith "add score"
           else score
       | _ -> score in *)

    (* Decrease score if we are attacked by an enemy pawn. Remaining part
       of threat evaluation must be done later when we have full attack info. *)
    let score =
      if
        Utils.matrix_get
          ei.attacked_by
          (Types.colour_to_enum them)
          (Types.piece_type_to_enum Types.PAWN)
        |> BB.sq_and_bb sq
        |> BB.bb_not_zero
      then
        Score.Infix.(
          score
          - Array.get threatened_by_pawn_penalty (Types.piece_type_to_enum piece_type))
      else score
    in
    (* TODO: Finish other stuff *)
    ( attacked_by
    , king_attackers_count
    , king_attackers_weight
    , king_adjacent_zone_attacks_count
    , mobility
    , score )
  in
  let ( attacked_by
      , king_attackers_count
      , king_attackers_weight
      , king_adjacent_zone_attacks_count
      , mobility
      , score )
    =
    BB.fold_sq ~init:(BB.empty, 0, 0, 0, Score.zero, Score.zero) ~f:do_sq piece_bb
  in
  let ei =
    set_attacked_by ei us piece_type attacked_by
    |> set_king_attackers_count us king_attackers_count
    |> set_king_attackers_weight us king_attackers_weight
    |> set_king_adjacent_zone_attacks_count us king_adjacent_zone_attacks_count
  in
  ei, score, mobility
;;

let evaluate_pieces_of_color pos ei us =
  let them = Types.other_colour us in
  (* Do not include in mobility squares protected by enemy pawns or occupied by
     our pieces *)
  let mobility_area =
    BB.bb_not @@ BB.bb_or (get_attacked_by ei them Types.PAWN) (P.pieces_of_colour pos us)
  in
  List.fold
    [ Types.KNIGHT; Types.BISHOP; Types.ROOK; Types.QUEEN ]
    ~init:(ei, Score.zero, Score.zero)
    ~f:(fun (ei, score, mobility) pt ->
      let ei, score', mobility' = evaluate_pieces_of_color pos ei us pt mobility_area in
      ei, Score.add score score', Score.add mobility mobility')
;;

(* TODO: Do we need all attacked squares? *)

(*
   * Evaluate is the evaluator for the outer world. It returns a static evaluation
 * of the position from the point of view of the side to move.
*)
let evaluate pos _optimism =
  (* TODO: once we fix quiscence search, bring back this assert*)
  (* assert (BB.bb_is_empty @@ P.checkers pos); *)
  let score = P.psq_score pos in
  let simple_eval = simple_eval pos @@ Types.WHITE in
  let tempo_score =
    match P.side_to_move pos with
    | WHITE -> tempo
    | BLACK -> Score.neg tempo
  in
  let score = Score.Infix.(score + Score.mk simple_eval simple_eval + tempo_score) in
  (* Initialize attack and king safety bitboards *)
  let ei =
    mk_eval_info () |> init_eval_info pos Types.WHITE |> init_eval_info pos Types.BLACK
  in
  let ei, score', _mobility_white = evaluate_pieces_of_color pos ei Types.WHITE in
  let _ei, score'', _mobility_black = evaluate_pieces_of_color pos ei Types.BLACK in
  let score = Score.Infix.(score + score' - score'') in
  (* TODO: Figure out this scale factor business *)
  let score = interpolate score (Material.game_phase pos) Material.scale_factor_normal in
  if Types.equal_colour (P.side_to_move pos) Types.WHITE then score else -score
;;
