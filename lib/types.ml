(*
   Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2024 The Stockfish developers (see AUTHORS file)

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
*)

open Base

(* TODO: Make a module out of this to prevent exposing unnecessary details *)
module Types = struct
  type colour =
    | WHITE
    | BLACK
  [@@deriving enum, eq, ord, sexp]

  let other_colour = function
    | WHITE -> BLACK
    | BLACK -> WHITE
  ;;

  (* TODO: We can represent this as a bit-wise enum when we need it. *)
  type castling_right =
    | WHITE_OO
    | WHITE_OOO
    | BLACK_OO
    | BLACK_OOO
  [@@deriving sexp, ord, hash]

  let all_castling_rights = [ WHITE_OO; WHITE_OOO; BLACK_OO; BLACK_OOO ]

  let castling_right_to_enum = function
    | WHITE_OO -> 0b1
    | WHITE_OOO -> 0b10
    | BLACK_OO -> 0b100
    | BLACK_OOO -> 0b1000
  ;;

  let castling_rights_for_colour = function
    | WHITE -> [ WHITE_OO; WHITE_OOO ]
    | BLACK -> [ BLACK_OO; BLACK_OOO ]
  ;;

  let castling_right_num_combinations = 2 ** 4

  let is_kingside_castling = function
    | WHITE_OO | BLACK_OO -> true
    | WHITE_OOO | BLACK_OOO -> false
  ;;

  let mk_castling_right ~color ~is_kingside =
    match color, is_kingside with
    | WHITE, true -> WHITE_OO
    | WHITE, false -> WHITE_OOO
    | BLACK, true -> BLACK_OO
    | BLACK, false -> BLACK_OOO
  ;;

  (* Value represents a search value. The values used in search are always
   * supposed to be in the range (-value_none| value_none] and should not
   * exceed this range. *)

  type value = int

  (* TODO: All numbers here are `values`| remove this TODO after I make this
     a module. *)

  let max_moves = 256
  let max_ply = 25

  (* let max_ply = 246 *)
  let value_zero = 0
  let value_draw = 0
  let value_none = 32002
  let value_infinite = 32001
  let value_mate = 32000
  let value_mate_in_max_ply = value_mate - max_ply
  let value_mated_in_max_ply = -value_mate_in_max_ply

  (* TODO: What is `tb`? *)
  let value_tb = value_mate_in_max_ply - 1
  let value_tb_win_in_max_ply = value_tb - max_ply
  let value_tb_loss_in_max_ply = -value_tb_win_in_max_ply
  let pawn_value = 208
  let knight_value = 781
  let bishop_value = 825
  let rook_value = 1276
  let queen_value = 2538

  (* Value of a minor piece and rook *)
  let futility_margin_1 = (knight_value + bishop_value) / 2
  let futility_margin_2 = rook_value

  (* TODO: Check if we need an enum of no piece and all pieces types. Wouldn't
     it be nicer to represent the absence of pieces using a None? *)
  type piece_type =
    | PAWN
    | KNIGHT
    | BISHOP
    | ROOK
    | QUEEN
    | KING
  [@@deriving enum, sexp, ord, eq, show]

  let all_piece_types = [ PAWN; KNIGHT; BISHOP; ROOK; QUEEN; KING ]

  (* TODO: Check if the fancy integer arithmetic here is really necessary.
     Probably not actually| the point of this rewrite is to eliminate all the weird C++ stuff. *)
  type piece =
    | W_PAWN
    | W_KNIGHT
    | W_BISHOP
    | W_ROOK
    | W_QUEEN
    | W_KING
    | B_PAWN
    | B_KNIGHT
    | B_BISHOP
    | B_ROOK
    | B_QUEEN
    | B_KING
  [@@deriving enum, eq, show, ord, sexp]

  let all_pieces =
    [ W_PAWN
    ; W_KNIGHT
    ; W_BISHOP
    ; W_ROOK
    ; W_QUEEN
    ; W_KING
    ; B_PAWN
    ; B_KNIGHT
    ; B_BISHOP
    ; B_ROOK
    ; B_QUEEN
    ; B_KING
    ]
  ;;

  let piece_value = function
    | W_PAWN | B_PAWN -> pawn_value
    | W_KNIGHT | B_KNIGHT -> knight_value
    | W_BISHOP | B_BISHOP -> bishop_value
    | W_ROOK | B_ROOK -> rook_value
    | W_QUEEN | B_QUEEN -> queen_value
    | W_KING | B_KING -> value_zero
  ;;

  let piece_type_value = function
    | PAWN -> pawn_value
    | KNIGHT -> knight_value
    | BISHOP -> bishop_value
    | ROOK -> rook_value
    | QUEEN -> queen_value
    | KING -> value_zero
  ;;

  type square =
    | A1
    | B1
    | C1
    | D1
    | E1
    | F1
    | G1
    | H1
    | A2
    | B2
    | C2
    | D2
    | E2
    | F2
    | G2
    | H2
    | A3
    | B3
    | C3
    | D3
    | E3
    | F3
    | G3
    | H3
    | A4
    | B4
    | C4
    | D4
    | E4
    | F4
    | G4
    | H4
    | A5
    | B5
    | C5
    | D5
    | E5
    | F5
    | G5
    | H5
    | A6
    | B6
    | C6
    | D6
    | E6
    | F6
    | G6
    | H6
    | A7
    | B7
    | C7
    | D7
    | E7
    | F7
    | G7
    | H7
    | A8
    | B8
    | C8
    | D8
    | E8
    | F8
    | G8
    | H8
  [@@deriving enum, sexp, ord, eq, show { with_path = false }]

  let all_squares =
    [ A1
    ; B1
    ; C1
    ; D1
    ; E1
    ; F1
    ; G1
    ; H1
    ; A2
    ; B2
    ; C2
    ; D2
    ; E2
    ; F2
    ; G2
    ; H2
    ; A3
    ; B3
    ; C3
    ; D3
    ; E3
    ; F3
    ; G3
    ; H3
    ; A4
    ; B4
    ; C4
    ; D4
    ; E4
    ; F4
    ; G4
    ; H4
    ; A5
    ; B5
    ; C5
    ; D5
    ; E5
    ; F5
    ; G5
    ; H5
    ; A6
    ; B6
    ; C6
    ; D6
    ; E6
    ; F6
    ; G6
    ; H6
    ; A7
    ; B7
    ; C7
    ; D7
    ; E7
    ; F7
    ; G7
    ; H7
    ; A8
    ; B8
    ; C8
    ; D8
    ; E8
    ; F8
    ; G8
    ; H8
    ]
  ;;

  type direction =
    | NORTH
    | EAST
    | SOUTH
    | WEST
    | NORTH_EAST
    | SOUTH_EAST
    | SOUTH_WEST
    | NORTH_WEST

  let all_directions =
    [ NORTH; EAST; SOUTH; WEST; NORTH_EAST; SOUTH_EAST; SOUTH_WEST; NORTH_WEST ]
  ;;

  let direction_to_enum = function
    | NORTH -> 8
    | EAST -> 1
    | SOUTH -> -8 (* -NORTH *)
    | WEST -> -1 (* -EAST *)
    | NORTH_EAST -> 9 (* NORTH + EAST *)
    | SOUTH_EAST -> -7 (* SOUTH + EAST *)
    | SOUTH_WEST -> -9 (* SOUTH + WEST *)
    | NORTH_WEST -> 7 (* NORTH + WEST *)
  ;;

  let direction_of_enum = function
    | 8 -> NORTH
    | -8 -> SOUTH
    | -1 -> WEST
    | 9 -> NORTH_EAST
    | -7 -> SOUTH_EAST
    | -9 -> SOUTH_WEST
    | 7 -> NORTH_WEST
    | _ -> failwith "Invalid direction"
  ;;

  type knight_direction =
    | SOUTH_SOUTH_EAST
    | SOUTH_SOUTH_WEST
    | SOUTH_WEST_WEST
    | SOUTH_EAST_EAST
    | NORTH_NORTH_WEST
    | NORTH_NORTH_EAST
    | NORTH_EAST_EAST
    | NORTH_WEST_WEST

  let all_knight_directions =
    [ SOUTH_SOUTH_EAST
    ; SOUTH_SOUTH_WEST
    ; SOUTH_WEST_WEST
    ; SOUTH_EAST_EAST
    ; NORTH_NORTH_WEST
    ; NORTH_NORTH_EAST
    ; NORTH_EAST_EAST
    ; NORTH_WEST_WEST
    ]
  ;;

  let knight_direction_to_enum = function
    | SOUTH_SOUTH_EAST -> -17
    | SOUTH_SOUTH_WEST -> -15
    | SOUTH_WEST_WEST -> -10
    | SOUTH_EAST_EAST -> -6
    | NORTH_NORTH_WEST -> 15
    | NORTH_NORTH_EAST -> 17
    | NORTH_EAST_EAST -> 10
    | NORTH_WEST_WEST -> 6
  ;;

  let knight_direction_of_enum = function
    | -17 -> SOUTH_SOUTH_EAST
    | -15 -> SOUTH_SOUTH_WEST
    | -10 -> SOUTH_WEST_WEST
    | -6 -> SOUTH_EAST_EAST
    | 15 -> NORTH_NORTH_WEST
    | 17 -> NORTH_NORTH_EAST
    | 10 -> NORTH_EAST_EAST
    | 6 -> NORTH_WEST_WEST
    | _ -> failwith "Invalid knight direction"
  ;;

  type file =
    | FILE_A
    | FILE_B
    | FILE_C
    | FILE_D
    | FILE_E
    | FILE_F
    | FILE_G
    | FILE_H
  [@@deriving enum, sexp, ord, eq]

  let all_files = [ FILE_A; FILE_B; FILE_C; FILE_D; FILE_E; FILE_F; FILE_G; FILE_H ]

  type rank =
    | RANK_1
    | RANK_2
    | RANK_3
    | RANK_4
    | RANK_5
    | RANK_6
    | RANK_7
    | RANK_8
  [@@deriving enum, sexp, ord, eq]

  let all_ranks = [ RANK_1; RANK_2; RANK_3; RANK_4; RANK_5; RANK_6; RANK_7; RANK_8 ]

  let file_of_ch = function
    | 'a' -> Some FILE_A
    | 'b' -> Some FILE_B
    | 'c' -> Some FILE_C
    | 'd' -> Some FILE_D
    | 'e' -> Some FILE_E
    | 'f' -> Some FILE_F
    | 'g' -> Some FILE_G
    | 'h' -> Some FILE_H
    | _ -> None
  ;;

  let sqs_of_rank rank =
    let base = rank_to_enum rank * 8 in
    List.rev
    @@ List.fold
         ~init:[]
         ~f:(fun acc i -> (Stdlib.Option.get @@ square_of_enum @@ (base + i)) :: acc)
         Utils.(0 -- 7)
  ;;

  let rank_of_int i = rank_of_enum (i - 1)

  (* TODO: This needs to do more checks, e.g. H4 + EAST should be invalid *)
  let sq_plus_dir sq dir = square_to_enum sq + direction_to_enum dir |> square_of_enum

  let sq_plus_dir_twice sq dir =
    match sq_plus_dir sq dir with
    | Some res -> sq_plus_dir res dir
    | None -> None
  ;;

  let sq_sub_dir sq dir = square_to_enum sq - direction_to_enum dir |> square_of_enum

  let sq_sub_dir_twice sq dir =
    match sq_sub_dir sq dir with
    | Some res -> sq_sub_dir res dir
    | None -> None
  ;;

  let shift_sq_exn dir n sq =
    let helper sq =
      match sq with
      | None -> None
      | Some sq -> sq_plus_dir sq dir
    in
    Fn.apply_n_times ~n helper (Some sq) |> Stdlib.Option.get
  ;;

  (* Swap A1 <-> A8 *)
  let flip_sq_rank sq =
    Int.bit_xor (square_to_enum sq) (square_to_enum A8)
    |> square_of_enum
    |> Stdlib.Option.get
  ;;

  (* Swap A1 <-> H1 *)
  let flip_sq_file sq =
    Int.bit_xor (square_to_enum sq) (square_to_enum H1)
    |> square_of_enum
    |> Stdlib.Option.get
  ;;

  let flip_piece_colour = function
    | W_PAWN -> B_PAWN
    | W_BISHOP -> B_BISHOP
    | W_ROOK -> B_ROOK
    | W_KNIGHT -> B_KNIGHT
    | W_QUEEN -> B_QUEEN
    | W_KING -> B_KING
    | B_PAWN -> W_PAWN
    | B_BISHOP -> W_BISHOP
    | B_ROOK -> W_ROOK
    | B_KNIGHT -> W_KNIGHT
    | B_QUEEN -> W_QUEEN
    | B_KING -> W_KING
  ;;

  let mk_piece color piece_type =
    match color, piece_type with
    | WHITE, PAWN -> W_PAWN
    | WHITE, KNIGHT -> W_KNIGHT
    | WHITE, BISHOP -> W_BISHOP
    | WHITE, ROOK -> W_ROOK
    | WHITE, QUEEN -> W_QUEEN
    | WHITE, KING -> W_KING
    | BLACK, PAWN -> B_PAWN
    | BLACK, KNIGHT -> B_KNIGHT
    | BLACK, BISHOP -> B_BISHOP
    | BLACK, ROOK -> B_ROOK
    | BLACK, QUEEN -> B_QUEEN
    | BLACK, KING -> B_KING
  ;;

  let type_of_piece = function
    | W_PAWN -> PAWN
    | W_KNIGHT -> KNIGHT
    | W_BISHOP -> BISHOP
    | W_ROOK -> ROOK
    | W_QUEEN -> QUEEN
    | W_KING -> KING
    | B_PAWN -> PAWN
    | B_KNIGHT -> KNIGHT
    | B_BISHOP -> BISHOP
    | B_ROOK -> ROOK
    | B_QUEEN -> QUEEN
    | B_KING -> KING
  ;;

  let color_of_piece = function
    | W_PAWN -> WHITE
    | W_KNIGHT -> WHITE
    | W_BISHOP -> WHITE
    | W_ROOK -> WHITE
    | W_QUEEN -> WHITE
    | W_KING -> WHITE
    | B_PAWN -> BLACK
    | B_KNIGHT -> BLACK
    | B_BISHOP -> BLACK
    | B_ROOK -> BLACK
    | B_QUEEN -> BLACK
    | B_KING -> BLACK
  ;;

  let short_piece_name = function
    | W_PAWN -> "P"
    | W_KNIGHT -> "N"
    | W_BISHOP -> "B"
    | W_ROOK -> "R"
    | W_QUEEN -> "Q"
    | W_KING -> "K"
    | B_PAWN -> "p"
    | B_KNIGHT -> "n"
    | B_BISHOP -> "b"
    | B_ROOK -> "r"
    | B_QUEEN -> "q"
    | B_KING -> "k"
  ;;

  let piece_type_to_algebraic = function
    | PAWN -> ""
    | BISHOP -> "b"
    | KNIGHT -> "n"
    | ROOK -> "r"
    | QUEEN -> "q"
    | KING -> ""
  ;;

  let mk_square ~file ~rank =
    Int.shift_left (rank_to_enum rank) 3 + file_to_enum file
    |> square_of_enum
    |> Stdlib.Option.get
  ;;

  let file_of_sq sq =
    (* The 3 lower bits completely determine the rank of the square *)
    Int.bit_and (square_to_enum sq) 0b111 |> file_of_enum |> Stdlib.Option.get
  ;;

  let rank_of_sq sq =
    (* Everything after the 3 lower bits determine the rank *)
    Int.shift_right_logical (square_to_enum sq) 3 |> rank_of_enum |> Stdlib.Option.get
  ;;

  (* Find the 'equivalent' square from the perspective of the other colour, e.g.
     the black counterpart of the weak F2 square would be F7. *)
  let relative_sq colour sq =
    let colour_mask =
      match colour with
      | WHITE -> 0
      | BLACK -> 0b111000
    in
    Int.bit_xor (square_to_enum sq) colour_mask |> square_of_enum |> Stdlib.Option.get
  ;;

  let relative_rank colour rank =
    let colour_mask =
      match colour with
      | WHITE -> 0
      | BLACK -> 0b111
    in
    Int.bit_xor (rank_to_enum rank) colour_mask |> rank_of_enum |> Stdlib.Option.get
  ;;

  (* Given a square, give us the relative rank of it, e.g. the F7 square
     for BLACK is seen as being of the 2nd rank. *)
  let relative_rank_of_sq colour sq = relative_rank colour @@ rank_of_sq sq

  let pawn_push_direction = function
    | WHITE -> NORTH
    | BLACK -> SOUTH
  ;;

  let mate_in ply = value_mate - ply
  let mated_in ply = -value_mate + ply

  type move_type =
    | NORMAL
    | PROMOTION
    | EN_PASSANT
    | CASTLING
  [@@deriving enum, ord, eq, sexp, show]

  type move =
    { data : int
    ; value : int
    }
  [@@deriving sexp, eq]

  let equal_move m1 m2 = m1.data = m2.data
  let compare_move m1 m2 = compare m1.data m2.data

  (* A move needs 16 bits to be stored
     bit  0- 5: destination square (from 0 to 63)
     bit  6-11: origin square (from 0 to 63)
     bit 12-13: promotion piece type - 2 (from KNIGHT-2 to QUEEN-2)
     bit 14-15: special move flag: promotion (1), en passant (2), castling (3)

     NOTE: en passant bit is set only when a pawn can be captured *)
  (* TODO: If necessary, I can create another version of this function that
     handles the 'fast' case, i.e. for normal move types without ppt. *)
  let mk_move ?(move_type = NORMAL) ?(ppt = None) ?(value = 0) dst src =
    let special_move_flag = move_type_to_enum move_type in
    (* TODO: Perhaps it would be prudent to ensure that this only `Some`
       when move_type == PROMOTION *)
    let ppt_flag =
      match ppt with
      | Some KNIGHT -> 0
      | Some BISHOP -> 1
      | Some ROOK -> 2
      | Some QUEEN -> 3
      | None -> 0
      | _ -> failwith "Invalid promotion piece type"
    in
    (* Used for piping *)
    let shift_left = Fn.flip Int.shift_left in
    let data =
      Int.shift_left special_move_flag 2
      |> Int.bit_or ppt_flag
      |> shift_left 6
      |> Int.bit_or (square_to_enum src)
      |> shift_left 6
      |> Int.bit_or (square_to_enum dst)
    in
    { data; value }
  ;;

  let move_value { value; _ } = value
  let none_move = { data = 0; value = 0 }
  let null_move = { data = 0b1000001; value = 0 } (* Same src and dest *)
  let move_not_none move = not (equal_move move none_move)
  let move_is_none move = equal_move move none_move
  let move_not_null move = not (equal_move move null_move)
  let move_is_ok move = move_not_none move && move_not_null move

  let move_src { data; _ } =
    Int.shift_right_logical data 6
    |> Int.bit_and 0b111111
    |> square_of_enum
    |> Stdlib.Option.get
  ;;

  let move_dst { data; _ } =
    Int.bit_and data 0b111111 |> square_of_enum |> Stdlib.Option.get
  ;;

  let get_move_type { data; _ } =
    Int.shift_right data 14 |> move_type_of_enum |> Stdlib.Option.get
  ;;

  let get_ppt ({ data; _ } as m) =
    match get_move_type m, Int.shift_right data 12 |> Int.bit_and 0b11 with
    | PROMOTION, 0 -> Some KNIGHT
    | PROMOTION, 1 -> Some BISHOP
    | PROMOTION, 2 -> Some ROOK
    | PROMOTION, 3 -> Some QUEEN
    | _ -> None
  ;;

  let show_move move =
    if move_is_none move
    then "<none>"
    else if not @@ move_not_null move
    then "<null>"
    else
      Printf.sprintf
        "%s%s%s"
        (String.lowercase @@ show_square @@ move_src move)
        (String.lowercase @@ show_square @@ move_dst move)
        (get_ppt move |> Option.fold ~init:"" ~f:(fun _ pt -> piece_type_to_algebraic pt))
  ;;

  let depth_qs_checks = 0
  let depth_qs_no_checks = -1
  let depth_none = -6
  let depth_offset = -7 (* value used only for TT entry occupancy check *)

  let parse_algrebraic_sq sq =
    let open Option.Let_syntax in
    let res =
      match String.to_list sq with
      | [ file; rank ] ->
        let%bind rank = Stdlib.int_of_string_opt @@ String.make 1 rank in
        let%bind rank = rank_of_int rank in
        let%bind file = file_of_ch file in
        return @@ mk_square ~file ~rank
      | _ -> None
    in
    Result.of_option res ~error:"invalid algebraic square"
  ;;

  let distance_by_file sq1 sq2 =
    Int.abs (file_to_enum (file_of_sq sq1) - file_to_enum (file_of_sq sq2))
  ;;

  let distance_by_rank sq1 sq2 =
    Int.abs (rank_to_enum (rank_of_sq sq1) - rank_to_enum (rank_of_sq sq2))
  ;;
end

module SquareCmp = struct
  include
    (val Comparator.make ~compare:Types.compare_square ~sexp_of_t:Types.sexp_of_square)

  type t = Types.square
end

module PieceCmp = struct
  include
    (val Comparator.make ~compare:Types.compare_piece ~sexp_of_t:Types.sexp_of_piece)

  type t = Types.piece
end

module ColourCmp = struct
  include
    (val Comparator.make ~compare:Types.compare_colour ~sexp_of_t:Types.sexp_of_colour)

  type t = Types.colour
end

module PieceTypeCmp = struct
  include
    (val Comparator.make
           ~compare:Types.compare_piece_type
           ~sexp_of_t:Types.sexp_of_piece_type)

  type t = Types.piece_type
end

module CastlingRightCmp = struct
  include
    (val Comparator.make
           ~compare:Types.compare_castling_right
           ~sexp_of_t:Types.sexp_of_castling_right)

  type t = Types.castling_right
end

let%test_unit "test_sq_plus_dir" =
  [%test_result: Types.square option]
    ~expect:(Some Types.F5)
    (Types.sq_plus_dir Types.E4 Types.NORTH_EAST);
  [%test_result: Types.square option]
    ~expect:(Some Types.E6)
    (Types.sq_plus_dir Types.E5 Types.NORTH);
  [%test_result: Types.square option]
    ~expect:(Some Types.D5)
    (Types.sq_plus_dir Types.E4 Types.NORTH_WEST);
  [%test_result: Types.square option]
    ~expect:(Some Types.H7)
    (Types.sq_plus_dir Types.H8 Types.SOUTH);
  [%test_result: Types.square option]
    ~expect:(Some Types.D2)
    (Types.sq_plus_dir Types.C2 Types.EAST);
  [%test_result: Types.square option]
    ~expect:(Some Types.E7)
    (Types.sq_plus_dir Types.F7 Types.WEST);
  [%test_result: Types.square option]
    ~expect:(Some Types.D3)
    (Types.sq_plus_dir Types.E4 Types.SOUTH_WEST);
  [%test_result: Types.square option]
    ~expect:(Some Types.F3)
    (Types.sq_plus_dir Types.E4 Types.SOUTH_EAST);
  [%test_result: Types.square option]
    ~expect:None
    (Types.sq_plus_dir Types.H8 Types.NORTH);
  [%test_result: Types.square option]
    ~expect:None
    (Types.sq_plus_dir Types.H8 Types.NORTH_EAST);
  [%test_result: Types.square option]
    ~expect:None
    (Types.sq_plus_dir Types.H8 Types.NORTH_WEST);
  [%test_result: Types.square option] ~expect:None (Types.sq_plus_dir Types.H8 Types.EAST)
;;

(* TODO: Fix this! *)
(* [%test_result: Types.square option] ~expect:None *)
(* (Types.sq_plus_dir Types.H4 Types.EAST) *)

let%test_unit "test_flip_sq_rank" =
  [%test_result: Types.square] ~expect:Types.A8 (Types.flip_sq_rank Types.A1);
  [%test_result: Types.square] ~expect:Types.A1 (Types.flip_sq_rank Types.A8);
  [%test_result: Types.square] ~expect:Types.E7 (Types.flip_sq_rank Types.E2);
  [%test_result: Types.square] ~expect:Types.D4 (Types.flip_sq_rank Types.D5);
  [%test_result: Types.square] ~expect:Types.F6 (Types.flip_sq_rank Types.F3)
;;

let%test_unit "test_flip_sq_file" =
  [%test_result: Types.square] ~expect:Types.H1 (Types.flip_sq_file Types.A1);
  [%test_result: Types.square] ~expect:Types.B2 (Types.flip_sq_file Types.G2);
  [%test_result: Types.square] ~expect:Types.C3 (Types.flip_sq_file Types.F3);
  [%test_result: Types.square] ~expect:Types.D4 (Types.flip_sq_file Types.E4);
  [%test_result: Types.square] ~expect:Types.E5 (Types.flip_sq_file Types.D5)
;;

let%test_unit "test_mk_square" =
  [%test_result: Types.square]
    ~expect:Types.E5
    (Types.mk_square ~file:Types.FILE_E ~rank:Types.RANK_5);
  [%test_result: Types.square]
    ~expect:Types.H8
    (Types.mk_square ~file:Types.FILE_H ~rank:Types.RANK_8);
  [%test_result: Types.square]
    ~expect:Types.A1
    (Types.mk_square ~file:Types.FILE_A ~rank:Types.RANK_1)
;;

let%test_unit "test_file_of_sq" =
  [%test_result: Types.file] ~expect:Types.FILE_E (Types.file_of_sq Types.E4);
  [%test_result: Types.file] ~expect:Types.FILE_F (Types.file_of_sq Types.F5);
  [%test_result: Types.file] ~expect:Types.FILE_G (Types.file_of_sq Types.G6);
  [%test_result: Types.file] ~expect:Types.FILE_H (Types.file_of_sq Types.H6)
;;

let%test_unit "test_rank_of_sq" =
  [%test_result: Types.rank] ~expect:Types.RANK_4 (Types.rank_of_sq Types.E4);
  [%test_result: Types.rank] ~expect:Types.RANK_5 (Types.rank_of_sq Types.F5);
  [%test_result: Types.rank] ~expect:Types.RANK_6 (Types.rank_of_sq Types.G6);
  [%test_result: Types.rank] ~expect:Types.RANK_7 (Types.rank_of_sq Types.G7)
;;

let%test_unit "test_relative_sq" =
  [%test_result: Types.square] ~expect:Types.E5 (Types.relative_sq Types.BLACK Types.E4);
  [%test_result: Types.square] ~expect:Types.A7 (Types.relative_sq Types.BLACK Types.A2);
  [%test_result: Types.square] ~expect:Types.F2 (Types.relative_sq Types.BLACK Types.F7)
;;

let%test_unit "test_relative_rank" =
  [%test_result: Types.rank]
    ~expect:Types.RANK_8
    (Types.relative_rank Types.BLACK Types.RANK_1);
  [%test_result: Types.rank]
    ~expect:Types.RANK_3
    (Types.relative_rank Types.BLACK Types.RANK_6)
;;

let%test_unit "test_normal_pawn_move" =
  let move = Types.mk_move Types.E5 Types.E4 in
  [%test_result: Types.square] ~expect:Types.E4 (Types.move_src move);
  [%test_result: Types.square] ~expect:Types.E5 (Types.move_dst move);
  [%test_result: Types.move_type] ~expect:Types.NORMAL (Types.get_move_type move)
;;

let%test_unit "test_construct_and_deconstruct_move_not_a_promotion" =
  let move =
    Types.mk_move ~ppt:(Some Types.QUEEN) Types.E4 Types.E5 ~move_type:Types.EN_PASSANT
  in
  [%test_result: Types.square] ~expect:Types.E5 (Types.move_src move);
  [%test_result: Types.square] ~expect:Types.E4 (Types.move_dst move);
  [%test_result: Types.move_type] ~expect:Types.EN_PASSANT (Types.get_move_type move);
  [%test_result: Types.piece_type option] ~expect:None (Types.get_ppt move)
;;

let%test_unit "test_construct_and_deconstruct_move_is_promotion" =
  let move =
    Types.mk_move ~ppt:(Some Types.QUEEN) Types.C8 Types.B7 ~move_type:Types.PROMOTION
  in
  [%test_result: Types.square] ~expect:Types.B7 (Types.move_src move);
  [%test_result: Types.square] ~expect:Types.C8 (Types.move_dst move);
  [%test_result: Types.move_type] ~expect:Types.PROMOTION (Types.get_move_type move);
  [%test_result: Types.piece_type option] ~expect:(Some Types.QUEEN) (Types.get_ppt move)
;;

let%test_unit "test_move_is_ok" =
  [%test_eq: bool] (Types.move_is_ok Types.null_move) false;
  [%test_eq: bool] (Types.move_is_ok Types.none_move) false;
  [%test_eq: bool]
    (Types.move_is_ok
     @@ Types.mk_move ~ppt:(Some Types.QUEEN) ~move_type:Types.PROMOTION Types.F7 Types.H8
    )
    true
;;

let%test_unit "squares_of_rank" =
  [%test_eq: Types.square list]
    (Types.sqs_of_rank Types.RANK_5)
    [ Types.A5; Types.B5; Types.C5; Types.D5; Types.E5; Types.F5; Types.G5; Types.H5 ]
;;
