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
open Unsigned
open Types
open Utils

module Bitboard : sig
  type t

  val show : t -> string
  val sexp_of_t : t -> Sexp.t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val empty : t
  val file_A : t
  val file_B : t
  val file_C : t
  val file_D : t
  val file_E : t
  val file_F : t
  val file_G : t
  val file_H : t
  val rank_1 : t
  val rank_2 : t
  val rank_3 : t
  val rank_4 : t
  val rank_5 : t
  val rank_6 : t
  val rank_7 : t
  val rank_8 : t
  val square_bb : Types.square -> t
  val bb_and : t -> t -> t
  val bb_or : t -> t -> t
  val bb_xor : t -> t -> t
  val bb_not : t -> t
  val bb_and_sq : t -> Types.square -> t
  val sq_and_bb : Types.square -> t -> t
  val bb_or_sq : t -> Types.square -> t
  val sq_or_bb : Types.square -> t -> t
  val bb_xor_sq : t -> Types.square -> t
  val sq_xor_bb : Types.square -> t -> t
  val more_than_one : t -> bool
  val bb_not_zero : t -> bool
  val bb_is_empty : t -> bool
  val rank_bb : Types.rank -> t
  val rank_bb_of_sq : Types.square -> t
  val file_bb : Types.file -> t
  val file_bb_of_sq : Types.square -> t
  val shift : t -> Types.direction -> t

  (* bb contains the positions of all pawns of the given color *)
  val pawn_attacks_bb : Types.colour -> t -> t

  (* Squares attacked by a pawn of a given colour on a given square *)
  val pawn_attacks_bb_from_sq : Types.colour -> Types.square -> t

  (*
     * Returns a bitboard representing an entire line (from board edge
   * to board edge) that intersects the two given squares. If the given squares
   * are not on a same file/rank/diagonal, the function returns 0. For instance,
   * line_bb(SQ_C4, SQ_F7) will return a bitboard with the A2-G8 diagonal.
  *)
  val line_bb : Types.square -> Types.square -> t

  (*
     * Returns a bitboard representing the squares in the semi-open
   * segment between the squares s1 and s2 (excluding s1 but including s2). If the
   * given squares are not on a same file/rank/diagonal, it returns s2. For instance,
   * between_bb(SQ_C4, SQ_F7) will return a bitboard with squares D5, E6 and F7, but
   * between_bb(SQ_E6, SQ_F8) will return a bitboard with the square F8. This trick
   * allows to generate non-king evasion moves faster: the defending piece must either
   * interpose itself to cover the check or capture the checking piece.
  *)
  val between_bb : Types.square -> Types.square -> t

  (*
     * Returns true if the squares s1, s2 and s3 are aligned either on a
   * straight or on a diagonal line.
  *)
  val aligned : Types.square -> Types.square -> Types.square -> bool
  val lsb : t -> t
  val msb : t -> t
  val pop_lsb : t -> t * t
  val sq_distance : Types.square -> Types.square -> int
  val attacks_bb : Types.piece_type -> Types.square -> t
  val attacks_bb_occupied : Types.piece_type -> Types.square -> t -> t

  (* Takes a bitboard containing a single set bit and transforms it to a square *)
  val bb_to_square : t -> Types.square

  (* Number of pieces on a certain bitboard *)
  val popcount : t -> int
  val fold : init:'a -> f:('a -> t -> 'a) -> t -> 'a
  val fold_sq : init:'a -> f:('a -> Types.square -> 'a) -> t -> 'a

  module Infix : sig
      type t

      val ( & ) : t -> t -> t
      val ( || ) : t -> t -> t
      val ( ^ ) : t -> t -> t
    end
    with type t := t
end = struct
  type t = UInt64.t

  let sexp_of_t t = Sexp.Atom (UInt64.to_string t)
  let compare = UInt64.compare
  let empty = UInt64.zero
  let equal = UInt64.equal

  (* The first of every 8 bits is set, this is repeated 8 times *)
  let file_A = UInt64.of_int 0x0101010101010101
  let file_B = UInt64.shift_left file_A 1
  let file_C = UInt64.shift_left file_A 2
  let file_D = UInt64.shift_left file_A 3
  let file_E = UInt64.shift_left file_A 4
  let file_F = UInt64.shift_left file_A 5
  let file_G = UInt64.shift_left file_A 6
  let file_H = UInt64.shift_left file_A 7

  (* First 8 bits are set *)
  let rank_1 = UInt64.of_int 0xFF
  let rank_2 = UInt64.shift_left rank_1 (8 * 1)
  let rank_3 = UInt64.shift_left rank_1 (8 * 2)
  let rank_4 = UInt64.shift_left rank_1 (8 * 3)
  let rank_5 = UInt64.shift_left rank_1 (8 * 4)
  let rank_6 = UInt64.shift_left rank_1 (8 * 5)
  let rank_7 = UInt64.shift_left rank_1 (8 * 6)
  let rank_8 = UInt64.shift_left rank_1 (8 * 7)
  let square_bb sq = UInt64.shift_left (UInt64.of_int 1) (Types.square_to_enum sq)
  let bb_and = UInt64.logand
  let bb_or = UInt64.logor
  let bb_xor = UInt64.logxor
  let bb_not = UInt64.lognot
  let bb_and_sq bb sq = UInt64.logand bb @@ square_bb sq
  let sq_and_bb = Fn.flip bb_and_sq
  let bb_or_sq bb sq = UInt64.logor bb @@ square_bb sq
  let sq_or_bb = Fn.flip bb_or_sq
  let bb_xor_sq bb sq = UInt64.logxor bb @@ square_bb sq
  let sq_xor_bb = Fn.flip bb_xor_sq
  let bb_not_zero bb = not @@ UInt64.equal bb UInt64.zero
  let bb_is_empty bb = UInt64.equal bb UInt64.zero

  (* b & (b - 1) pops the least significant bit, so if the result is not zero
     then there was more than 1 bit in the original bb *)
  let more_than_one bb = bb_not_zero (UInt64.logand bb @@ UInt64.pred bb)

  let rank_bb = function
    | Types.RANK_1 -> rank_1
    | Types.RANK_2 -> rank_2
    | Types.RANK_3 -> rank_3
    | Types.RANK_4 -> rank_4
    | Types.RANK_5 -> rank_5
    | Types.RANK_6 -> rank_6
    | Types.RANK_7 -> rank_7
    | Types.RANK_8 -> rank_8
  ;;

  let rank_bb_of_sq sq = Types.rank_of_sq sq |> rank_bb

  let file_bb = function
    | Types.FILE_A -> file_A
    | Types.FILE_B -> file_B
    | Types.FILE_C -> file_C
    | Types.FILE_D -> file_D
    | Types.FILE_E -> file_E
    | Types.FILE_F -> file_F
    | Types.FILE_G -> file_G
    | Types.FILE_H -> file_H
  ;;

  let file_bb_of_sq sq = Types.file_of_sq sq |> file_bb

  let shift bb dir =
    match dir with
    | Types.NORTH -> UInt64.shift_left bb 8
    | Types.EAST ->
      (*  (bb & ~file_H) << 1 *)
      UInt64.logand bb @@ UInt64.lognot file_H |> Fn.flip UInt64.shift_left 1
    | Types.SOUTH -> UInt64.shift_right bb 8
    | Types.WEST ->
      (*  (bb & ~file_A) >> 1 *)
      UInt64.logand bb @@ UInt64.lognot file_A |> Fn.flip UInt64.shift_right 1
    | Types.NORTH_EAST ->
      UInt64.logand bb @@ UInt64.lognot file_H |> Fn.flip UInt64.shift_left 9
    | Types.SOUTH_EAST ->
      (*  (bb & ~file_H) >> 7 *)
      UInt64.logand bb @@ UInt64.lognot file_H |> Fn.flip UInt64.shift_right 7
    | Types.SOUTH_WEST ->
      (*  (bb & ~file_A) >> 9 *)
      UInt64.logand bb @@ UInt64.lognot file_A |> Fn.flip UInt64.shift_right 9
    | Types.NORTH_WEST ->
      (*  (bb & ~file_A) << 7 *)
      UInt64.logand bb @@ UInt64.lognot file_A |> Fn.flip UInt64.shift_left 7
  ;;

  let lsb bb =
    (* Instead of doing bb & -bb, we do bb & (~bb + 1) *)
    UInt64.logand bb (UInt64.lognot bb |> UInt64.succ)
  ;;

  let msb bb =
    let bb = UInt64.logor bb (UInt64.shift_right bb 1) in
    let bb = UInt64.logor bb (UInt64.shift_right bb 2) in
    let bb = UInt64.logor bb (UInt64.shift_right bb 4) in
    let bb = UInt64.logor bb (UInt64.shift_right bb 8) in
    let bb = UInt64.logor bb (UInt64.shift_right bb 16) in
    let bb = UInt64.logor bb (UInt64.shift_right bb 32) in
    UInt64.logor
      (UInt64.shift_right (UInt64.succ bb) 1)
      (UInt64.logand bb (UInt64.shift_left UInt64.one 63))
  ;;

  let pop_lsb bb =
    let lsb = lsb bb in
    lsb, UInt64.logand bb @@ UInt64.lognot lsb
  ;;

  let bb_to_square bb =
    let rec log_2 n =
      if UInt64.compare n UInt64.one > 0
      then UInt64.shift_right n 1 |> log_2 |> UInt64.succ
      else UInt64.zero
    in
    assert (not @@ more_than_one bb);
    assert (bb_not_zero bb);
    log_2 bb |> UInt64.to_int |> Types.square_of_enum |> Stdlib.Option.get
  ;;

  let show bb =
    let res =
      List.fold
        ~init:"+---+---+---+---+---+---+---+---+\n"
        ~f:(fun acc rank ->
          let res =
            List.fold
              ~init:acc
              ~f:(fun acc file ->
                let to_add =
                  if bb_not_zero @@ bb_and_sq bb (Types.mk_square ~file ~rank)
                  then "| X "
                  else "|   "
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

  (* Initialisation of bitboards *)

  let distance_by_file sq1 sq2 =
    Int.abs
      (Types.file_to_enum (Types.file_of_sq sq1)
       - Types.file_to_enum (Types.file_of_sq sq2))
  ;;

  let distance_by_rank sq1 sq2 =
    Int.abs
      (Types.rank_to_enum (Types.rank_of_sq sq1)
       - Types.rank_to_enum (Types.rank_of_sq sq2))
  ;;

  let sq_distance_tbl =
    let tbl = Array.make_matrix ~dimx:64 ~dimy:64 0 in
    List.iter ~f:(fun (sq1, sq2) ->
      matrix_set
        tbl
        (Types.square_to_enum sq1)
        (Types.square_to_enum sq2)
        (Int.max (distance_by_file sq1 sq2) (distance_by_rank sq1 sq2)))
    @@ List.cartesian_product Types.all_squares Types.all_squares;
    tbl
  ;;

  let sq_distance sq1 sq2 =
    matrix_get sq_distance_tbl (Types.square_to_enum sq1) (Types.square_to_enum sq2)
  ;;

  (*
     * Returns the bitboard of target square for the given step (enum of direction)
   * from the given square. If the step is off the board, returns empty bitboard.
  *)
  let safe_destination sq step =
    let dst = Types.square_of_enum (Types.square_to_enum sq + step) in
    match dst with
    (* 2 because knight moves may switch the rank/file by 2. On the other
       hand, a valid step in any normal direction never changes the rank/file
       by 2 or more. *)
    | Some dst when sq_distance sq dst <= 2 -> square_bb dst
    | _ -> empty
  ;;

  let sliding_attack pt sq occupied =
    let rec inner (attacks, sq) direction =
      if
        bb_not_zero (safe_destination sq (Types.direction_to_enum direction))
        && (not @@ bb_not_zero (bb_and_sq occupied sq))
      then (
        let sq = Types.sq_plus_dir sq direction |> Stdlib.Option.get in
        let attacks = bb_or_sq attacks sq in
        inner (attacks, sq) direction)
      else attacks
    in
    let directions =
      match pt with
      | Types.ROOK -> [ Types.NORTH; Types.SOUTH; Types.EAST; Types.WEST ]
      | Types.BISHOP ->
        [ Types.NORTH_EAST; Types.SOUTH_EAST; Types.SOUTH_WEST; Types.NORTH_WEST ]
      | _ -> failwith "Invalid piece type"
    in
    List.fold ~init:UInt64.zero ~f:(fun attacks dir -> inner (attacks, sq) dir) directions
  ;;

  type magic =
    { mask : t
    ; magic : t
    ; attacks_offset : int (* Offset into the table for this particular magic *)
    ; shift : int
    }

  let empty_magic = { magic = empty; mask = empty; shift = 0; attacks_offset = 0 }

  let magic_index { mask; magic; shift; _ } occupied =
    (* ((occupied & mask) * magic) >> shift *)
    UInt64.Infix.((occupied land mask * magic) lsr shift) |> UInt64.to_int
  ;;

  (* Count number of set bits in UInt64 int *)
  let popcount b =
    let rec inner acc b =
      (* for (r = 0; b; r++, b &= b - 1) *)
      if bb_not_zero b then inner (acc + 1) UInt64.Infix.(b land UInt64.pred b) else acc
    in
    inner 0 b
  ;;

  (* Run this once and save the magic numbers to save computation time *)
  (* let compute_magics pt : magic array * t array =
       let random_u64_fewbits () =
         let random_u64 () =
           let u1 =
             Int64.(Random.bits64 () land of_int 0xffff) |> UInt64.of_int64
           in
           let u2 =
             Int64.(Random.bits64 () land of_int 0xffff) |> UInt64.of_int64
           in
           let u3 =
             Int64.(Random.bits64 () land of_int 0xffff) |> UInt64.of_int64
           in
           let u4 =
             Int64.(Random.bits64 () land of_int 0xffff) |> UInt64.of_int64
           in
           UInt64.Infix.(u1 lor (u2 lsl 16) lor (u3 lsl 32) lor (u4 lsl 48))
         in
         UInt64.Infix.(random_u64 () land random_u64 () land random_u64 ())
       in

       let attacks_tbl =
         match pt with
         | Types.BISHOP -> Array.create ~len:0x1480 empty
         | Types.ROOK -> Array.create ~len:0x19000 empty
         | _ -> failwith "Invalid piece type"
       in
       let magics_tbl = Array.create ~len:64 empty_magic in
       let occupancy = Array.create ~len:4096 empty in
       let reference = Array.create ~len:4096 empty in
       let epoch = Array.create ~len:4096 0 in

       let do_square (cnt, offset) sq =
         Random.init 42;
         let rec carry_rippler b size mask =
           Array.set occupancy size b;
           Array.set reference size (sliding_attack pt sq b);

           let b = UInt64.Infix.((b - mask) land mask) in
           let size = size + 1 in
           if bb_not_zero b then carry_rippler b size mask else size
         in
         let rec find_magic i size cnt m =
           let rec gen_magic () =
             let candidate = random_u64_fewbits () in
             (* popcount((m.magic * m.mask) >> 56) < 6 *)
             (* We need at least 6 set bits here *)
             if popcount UInt64.Infix.((candidate * m.mask) lsr 56) < 6 then
               gen_magic ()
             else candidate
           in
           let rec map_occupancies cnt i m =
             if i < size then
               let idx = magic_index m (Array.get occupancy i) in
               if Array.get epoch idx < cnt then (
                 Array.set epoch idx cnt;
                 Array.set attacks_tbl (offset + idx) @@ Array.get reference i;
                 map_occupancies cnt (i + 1) m)
               else if
                 not
                 @@ UInt64.equal
                      (Array.get attacks_tbl (offset + idx))
                      (Array.get reference i)
               then i
               else map_occupancies cnt (i + 1) m
             else i
           in
           if i < size then
             let magic = gen_magic () in
             let m = { m with magic } in
             let i = map_occupancies (cnt + 1) 0 m in
             find_magic i size (cnt + 1) m
           else (m, cnt)
         in

         (* We ignore squares on the edges when calculating piece occupancies,
            e.g. for a rook that is on E4, we don't care whether or not the A4
            square is occupied. *)
         let edges =
           (* ((Rank1BB | Rank8BB) & ~rank_bb(s)) | ((FileABB | FileHBB) & ~file_bb(s)) *)
           UInt64.logor
             (UInt64.logor rank_1 rank_8
             |> UInt64.logand @@ UInt64.lognot @@ rank_bb_of_sq sq)
             (UInt64.logor file_A file_H
             |> UInt64.logand @@ UInt64.lognot @@ file_bb_of_sq sq)
         in
         (* The mask includes all the squares that a piece attacks (while ignoring
            squares on the edges of the board). This is essentially the squares
            from which pieces may block the attacker. *)
         let mask =
           UInt64.logand (sliding_attack pt sq empty) (UInt64.lognot edges)
         in
         (* The index must be big enough to contain all possible subsets of the mask,
            hence we identify the number of non-zero bits in the mask to calculate
            how many bits we need to eventually shift to get the index. Note: the index
            refers to the N most significant bits of the 64bit integer. *)
         let shift = 64 - popcount mask in

         let m = { empty_magic with mask; shift; attacks_offset = offset } in
         let size = carry_rippler empty 0 mask in
         let m, cnt = find_magic 0 size cnt m in
         (cnt, offset + size, m)
       in
       ignore
       @@ List.fold_left ~init:(0, 0)
            ~f:(fun (cnt, offset) sq ->
              let cnt, offset, magic = do_square (cnt, offset) sq in
              Array.set magics_tbl (Types.square_to_enum sq) magic;
              (cnt, offset))
            Types.all_squares;
       Stdio.printf "Magic numbers for piece type %s\n" (Types.show_piece_type pt);
       List.iter Types.all_squares ~f:(fun sq ->
           Stdio.print_endline @@ UInt64.to_string
           @@ (Array.get magics_tbl (Types.square_to_enum sq)).magic);
       (magics_tbl, attacks_tbl)

     let _ = compute_magics Types.BISHOP
     let _ = compute_magics Types.ROOK *)

  (* The below magic numbers were obtained by executing `compute_magics` *)
  let bishop_magic_numbers =
    let numbers =
      [ "829805832255375376"
      ; "36600556017784840"
      ; "577701003651448834"
      ; "20304132011524612"
      ; "13836223675089657856"
      ; "282816080259072"
      ; "594585119226863617"
      ; "1153066642425579648"
      ; "1130641618387072"
      ; "598138658259202"
      ; "36361260519428"
      ; "297422431876808704"
      ; "2305862809294014466"
      ; "2305984331363387396"
      ; "4611695399846154240"
      ; "4611695399846154240"
      ; "2322238385162339"
      ; "2322238385162339"
      ; "299564159325307280"
      ; "10133202259697680"
      ; "72341921410321408"
      ; "81628040170310144"
      ; "12384942201899008"
      ; "2305984331363387396"
      ; "9842689571111043376"
      ; "9842689571111043376"
      ; "154249389039681792"
      ; "36600543132532744"
      ; "140876034736192"
      ; "9225624111681372672"
      ; "154249389039681792"
      ; "28221174082437632"
      ; "299144476361728"
      ; "1130457951388932"
      ; "1154047576312645696"
      ; "653024147139985536"
      ; "344527587701166336"
      ; "167134357406465"
      ; "2252920800411872"
      ; "1153066642425579648"
      ; "9306275275874304"
      ; "2305984331363387396"
      ; "10416896461739393280"
      ; "4612250351368701952"
      ; "2377918333149422080"
      ; "3463303437408534784"
      ; "1205067982046210"
      ; "4613940018342265352"
      ; "594585119226863617"
      ; "324331743358812288"
      ; "580965597761765634"
      ; "2305984331363387396"
      ; "31807015999898688"
      ; "1130641618387072"
      ; "829805832255375376"
      ; "36600556017784840"
      ; "1153066642425579648"
      ; "4611695399846154240"
      ; "1342072693318566976"
      ; "1176143194900138240"
      ; "36028798162011648"
      ; "145276409863995521"
      ; "1130641618387072"
      ; "829805832255375376"
      ]
    in
    let tbl = Array.create ~len:64 empty in
    List.iteri ~f:(fun i num -> Array.set tbl i (UInt64.of_string num)) numbers;
    tbl
  ;;

  let rook_magic_numbers =
    let numbers =
      [ "36028867888037890"
      ; "18014673655832576"
      ; "612524733829742720"
      ; "4683761208946592032"
      ; "144150389762162705"
      ; "180146186266869761"
      ; "5476385960173248642"
      ; "6953557972836500736"
      ; "140877074808832"
      ; "4629770787023233024"
      ; "3458905388756242432"
      ; "4757067878259228800"
      ; "140771848355968"
      ; "703696040100864"
      ; "4756082685775052804"
      ; "27303081346631936"
      ; "25475134695686144"
      ; "141288317919232"
      ; "72340168796799040"
      ; "2342013093611048961"
      ; "2342580991366857136"
      ; "704237231145984"
      ; "154578140994736898"
      ; "293026445876538501"
      ; "4756434535938293800"
      ; "112590266635919376"
      ; "144713341729768066"
      ; "43986910707842"
      ; "99087990042919042"
      ; "55173495629414528"
      ; "293369510679889936"
      ; "72072446037197393"
      ; "108231801612271649"
      ; "653022083680305216"
      ; "9655752796233797632"
      ; "1157433919662985216"
      ; "4611703632105048064"
      ; "289356825889882624"
      ; "13839851931415085712"
      ; "2316015691968283805"
      ; "36028934462128136"
      ; "4591561094479872"
      ; "1152956690069520402"
      ; "1170953495436624000"
      ; "288239172311875712"
      ; "4398080098432"
      ; "1263260803847684104"
      ; "422779405074433"
      ; "1188985765179162880"
      ; "6922036200817559616"
      ; "1152956696512073984"
      ; "4611721271535732992"
      ; "9224216479233736960"
      ; "4398080098432"
      ; "155375355409466368"
      ; "144255926654738560"
      ; "844974688059445"
      ; "74591143977058822"
      ; "2305878197914832961"
      ; "14429569491857836069"
      ; "2328923991748649090"
      ; "6922314274110965255"
      ; "1447951286704049412"
      ; "598688648930314"
      ]
    in
    let tbl = Array.create ~len:64 empty in
    List.iteri ~f:(fun i num -> Array.set tbl i (UInt64.of_string num)) numbers;
    tbl
  ;;

  let init_magics pt : magic array * t array =
    let attacks_tbl, magic_number_tbl =
      match pt with
      | Types.BISHOP -> Array.create ~len:0x1480 empty, bishop_magic_numbers
      | Types.ROOK -> Array.create ~len:0x19000 empty, rook_magic_numbers
      | _ -> failwith "Invalid piece type"
    in
    let magics_tbl = Array.create ~len:64 empty_magic in
    let do_square (cnt, offset) sq =
      let rec carry_rippler b size ({ mask; attacks_offset; _ } as m) =
        Array.set attacks_tbl (attacks_offset + magic_index m b) (sliding_attack pt sq b);
        let b = UInt64.Infix.((b - mask) land mask) in
        let size = size + 1 in
        if bb_not_zero b then carry_rippler b size m else size
      in
      (* We ignore squares on the edges when calculating piece occupancies,
         e.g. for a rook that is on E4, we don't care whether or not the A4
         square is occupied. *)
      let edges =
        (* ((Rank1BB | Rank8BB) & ~rank_bb(s)) | ((FileABB | FileHBB) & ~file_bb(s)) *)
        UInt64.logor
          (UInt64.logor rank_1 rank_8
           |> UInt64.logand @@ UInt64.lognot @@ rank_bb_of_sq sq)
          (UInt64.logor file_A file_H
           |> UInt64.logand @@ UInt64.lognot @@ file_bb_of_sq sq)
      in
      (* The mask includes all the squares that a piece attacks (while ignoring
         squares on the edges of the board). This is essentially the squares
         from which pieces may block the attacker. *)
      let mask = UInt64.logand (sliding_attack pt sq empty) (UInt64.lognot edges) in
      (* The index must be big enough to contain all possible subsets of the mask,
         hence we identify the number of non-zero bits in the mask to calculate
         how many bits we need to eventually shift to get the index. Note: the index
         refers to the N most significant bits of the 64bit integer. *)
      let shift = 64 - popcount mask in
      let magic_number = Array.get magic_number_tbl (Types.square_to_enum sq) in
      let m = { mask; shift; attacks_offset = offset; magic = magic_number } in
      let size = carry_rippler empty 0 m in
      cnt, offset + size, m
    in
    ignore
    @@ List.fold_left
         ~init:(0, 0)
         ~f:(fun (cnt, offset) sq ->
           let cnt, offset, magic = do_square (cnt, offset) sq in
           Array.set magics_tbl (Types.square_to_enum sq) magic;
           cnt, offset)
         Types.all_squares;
    magics_tbl, attacks_tbl
  ;;

  let bishop_magics, bishop_attacks = init_magics Types.BISHOP
  let rook_magics, rook_attacks = init_magics Types.ROOK

  let bishop_attacks_magical sq occupied =
    let ({ attacks_offset; _ } as magic) =
      Array.get bishop_magics (Types.square_to_enum sq)
    in
    let index = magic_index magic occupied in
    Array.get bishop_attacks (index + attacks_offset)
  ;;

  let rook_attacks_magical sq occupied =
    let ({ attacks_offset; _ } as magic) =
      Array.get rook_magics (Types.square_to_enum sq)
    in
    let index = magic_index magic occupied in
    Array.get rook_attacks (index + attacks_offset)
  ;;

  let pawn_attacks_bb colour bb =
    match colour with
    | Types.WHITE -> UInt64.logor (shift bb Types.NORTH_EAST) (shift bb Types.NORTH_WEST)
    | Types.BLACK -> UInt64.logor (shift bb Types.SOUTH_EAST) (shift bb Types.SOUTH_WEST)
  ;;

  (* Bitboard[2][64] *)
  let pawn_attacks =
    let tbl = Array.make_matrix ~dimx:2 ~dimy:(List.length Types.all_squares) empty in
    List.iter
      (List.cartesian_product Types.all_squares [ Types.WHITE; Types.BLACK ])
      ~f:(fun (sq, colour) ->
        matrix_set
          tbl
          (Types.colour_to_enum colour)
          (Types.square_to_enum sq)
          (pawn_attacks_bb colour @@ square_bb sq));
    tbl
  ;;

  let pawn_attacks_bb_from_sq colour sq =
    matrix_get pawn_attacks (Types.colour_to_enum colour) (Types.square_to_enum sq)
  ;;

  (* Bitboard[6][64]
     Squares that would be attacked by pieces if the board was completely empty. *)
  let pseudo_attacks =
    let tbl =
      Array.make_matrix
        ~dimx:(List.length Types.all_piece_types)
        ~dimy:(List.length Types.all_squares)
        empty
    in
    let do_square sq =
      let sq_enum = Types.square_to_enum sq in
      (* Setup king moves *)
      let king_tbl = Array.get tbl (Types.piece_type_to_enum Types.KING) in
      List.iter Types.all_directions ~f:(fun dir ->
        Array.set
          king_tbl
          sq_enum
          (UInt64.logor
             (Array.get king_tbl sq_enum)
             (safe_destination sq (Types.direction_to_enum dir))));
      let knight_tbl = Array.get tbl (Types.piece_type_to_enum Types.KNIGHT) in
      (* Setup knight moves *)
      List.iter Types.all_knight_directions ~f:(fun dir ->
        Array.set
          knight_tbl
          sq_enum
          (UInt64.logor
             (Array.get knight_tbl sq_enum)
             (safe_destination sq (Types.knight_direction_to_enum dir))));
      (* Setup bishop, rook, and queen moves at the same time *)
      let bishop_attacks = bishop_attacks_magical sq empty in
      let rook_attacks = rook_attacks_magical sq empty in
      matrix_set tbl (Types.piece_type_to_enum Types.BISHOP) sq_enum bishop_attacks;
      matrix_set tbl (Types.piece_type_to_enum Types.ROOK) sq_enum rook_attacks;
      matrix_set tbl (Types.piece_type_to_enum Types.QUEEN) sq_enum
      @@ UInt64.logor bishop_attacks rook_attacks
    in
    List.iter ~f:do_square Types.all_squares;
    tbl
  ;;

  let attacks_bb pt sq =
    assert (not @@ Types.equal_piece_type pt Types.PAWN);
    matrix_get pseudo_attacks (Types.piece_type_to_enum pt) (Types.square_to_enum sq)
  ;;

  let rec attacks_bb_occupied pt sq occupied =
    match pt with
    | Types.BISHOP -> bishop_attacks_magical sq occupied
    | Types.ROOK -> rook_attacks_magical sq occupied
    | Types.QUEEN ->
      UInt64.logor
        (attacks_bb_occupied Types.BISHOP sq occupied)
        (attacks_bb_occupied Types.ROOK sq occupied)
    | Types.PAWN -> failwith "Not allowed!"
    | _ ->
      matrix_get pseudo_attacks (Types.piece_type_to_enum pt) (Types.square_to_enum sq)
  ;;

  let line_bb_tbl, between_bb_tbl =
    let line_bb_tbl = Array.make_matrix ~dimx:64 ~dimy:64 empty in
    let between_bb_tbl = Array.make_matrix ~dimx:64 ~dimy:64 empty in
    List.iter ~f:(fun (sq1, sq2) ->
      let sq1_enum = Types.square_to_enum sq1 in
      let sq2_enum = Types.square_to_enum sq2 in
      List.iter
        ~f:(fun pt ->
          if
            bb_not_zero
            @@ bb_and_sq
                 (matrix_get pseudo_attacks (Types.piece_type_to_enum pt) sq1_enum)
                 sq2
          then (
            matrix_set line_bb_tbl sq1_enum sq2_enum
            @@ (UInt64.logand
                  (attacks_bb_occupied pt sq1 empty)
                  (attacks_bb_occupied pt sq2 empty)
                |> sq_or_bb sq1
                |> sq_or_bb sq2);
            matrix_set between_bb_tbl sq1_enum sq2_enum
            @@ UInt64.logand
                 (attacks_bb_occupied pt sq1 (square_bb sq2))
                 (attacks_bb_occupied pt sq2 (square_bb sq1))))
        [ Types.BISHOP; Types.ROOK ];
      matrix_set between_bb_tbl sq1_enum sq2_enum
      @@ bb_or_sq (matrix_get between_bb_tbl sq1_enum sq2_enum) sq2)
    @@ List.cartesian_product Types.all_squares Types.all_squares;
    line_bb_tbl, between_bb_tbl
  ;;

  let line_bb s1 s2 =
    matrix_get line_bb_tbl (Types.square_to_enum s1) (Types.square_to_enum s2)
  ;;

  let between_bb s1 s2 =
    matrix_get between_bb_tbl (Types.square_to_enum s1) (Types.square_to_enum s2)
  ;;

  let aligned s1 s2 s3 = bb_not_zero @@ bb_and_sq (line_bb s1 s2) s3

  let fold ~init ~f bb =
    let rec aux acc bb =
      if bb_not_zero bb
      then (
        let b, bb = pop_lsb bb in
        aux (f acc b) bb)
      else acc
    in
    aux init bb
  ;;

  let fold_sq ~init ~f bb = fold ~init ~f:(fun acc bb -> f acc @@ bb_to_square bb) bb

  module Infix = struct
    let ( & ) = bb_and
    let ( || ) = bb_or
    let ( ^ ) = bb_xor
  end

  (* Test of functions that we are not exposing outside of the module *)
  let%test_unit "test_lsb" =
    [%test_result: t] ~expect:(UInt64.of_int 0b0010) (lsb @@ UInt64.of_int 0b1010);
    [%test_result: t] ~expect:(UInt64.of_int 0) (lsb @@ UInt64.of_int 0);
    [%test_result: t] ~expect:(UInt64.of_int 1) (lsb @@ UInt64.of_int 1);
    [%test_result: t] ~expect:(UInt64.of_int 0b01000) (lsb @@ UInt64.of_int 0b01000)
  ;;

  let%test_unit "test_msb" =
    [%test_result: t] ~expect:(UInt64.of_int 0b00100000) (msb @@ UInt64.of_int 0b00110100);
    [%test_result: t] ~expect:(UInt64.of_int 0b0010) (msb @@ UInt64.of_int 0b0010);
    [%test_result: t] ~expect:(UInt64.of_int 0b1) (msb @@ UInt64.one);
    [%test_result: t] ~expect:(UInt64.of_int 0b0) (msb @@ UInt64.zero);
    [%test_result: t] ~expect:(UInt64.shift_left UInt64.one 63) (msb @@ UInt64.max_int)
  ;;

  let%test_unit "test_pop_lsb" =
    [%test_result: t * t]
      ~expect:(UInt64.of_int 0b100, UInt64.of_int 0b00110000)
      (pop_lsb @@ UInt64.of_int 0b00110100)
  ;;

  let%test_unit "test_popcount" =
    [%test_result: int] ~expect:64 (popcount UInt64.max_int);
    [%test_result: int]
      ~expect:34
      (popcount
       @@ UInt64.of_int 0b11110101010101010101111000001010100101010101010101010101011111)
  ;;

  let%test_unit "test_sliding_attack_bishop_empty_board" =
    let attacks = sliding_attack Types.BISHOP Types.A1 empty in
    let expected_attacked_squares =
      [ Types.B2; Types.C3; Types.D4; Types.E5; Types.F6; Types.G7; Types.H8 ]
    in
    let expected_bb =
      List.fold
        ~init:empty
        ~f:(fun acc sq -> UInt64.logor acc (square_bb sq))
        expected_attacked_squares
    in
    [%test_result: t] ~expect:expected_bb attacks
  ;;

  let%test_unit "test_sliding_attack_bishop_1_piece_obstructing_diagonal" =
    let occupied = square_bb Types.D4 in
    let attacks = sliding_attack Types.BISHOP Types.A1 occupied in
    let expected_attacked_squares = [ Types.B2; Types.C3; Types.D4 ] in
    let expected_bb =
      List.fold
        ~init:empty
        ~f:(fun acc sq -> UInt64.logor acc (square_bb sq))
        expected_attacked_squares
    in
    [%test_result: t] ~expect:expected_bb attacks
  ;;

  let%test_unit "test_sliding_attack_bishop_2_pieces_obstructing_diagonal" =
    let occupied = UInt64.Infix.(square_bb Types.D4 lor square_bb Types.E5) in
    let attacks = sliding_attack Types.BISHOP Types.A1 occupied in
    let expected_attacked_squares = [ Types.B2; Types.C3; Types.D4 ] in
    let expected_bb =
      List.fold
        ~init:empty
        ~f:(fun acc sq -> UInt64.logor acc (square_bb sq))
        expected_attacked_squares
    in
    [%test_result: t] ~expect:expected_bb attacks
  ;;

  let%test_unit "test_sliding_attack_rook_empty_board" =
    let attacks = sliding_attack Types.ROOK Types.E4 empty in
    let expected_attacked_squares =
      [ (* Down *)
        Types.E1
      ; Types.E2
      ; Types.E3
      ; (* Up *)
        Types.E5
      ; Types.E6
      ; Types.E7
      ; Types.E8
      ; (* Left *)
        Types.A4
      ; Types.B4
      ; Types.C4
      ; Types.D4
      ; (* Right *)
        Types.F4
      ; Types.G4
      ; Types.H4
      ]
    in
    let expected_bb =
      List.fold
        ~init:empty
        ~f:(fun acc sq -> UInt64.logor acc (square_bb sq))
        expected_attacked_squares
    in
    [%test_result: t] ~expect:expected_bb attacks
  ;;

  let%test_unit "test_sliding_attack_rook_1_piece_obstruction" =
    let occupied =
      UInt64.Infix.(square_bb Types.E5 lor square_bb Types.E6 lor square_bb Types.C4)
    in
    let attacks = sliding_attack Types.ROOK Types.E4 occupied in
    let expected_attacked_squares =
      [ (* Down *)
        Types.E1
      ; Types.E2
      ; Types.E3
      ; (* Up *)
        Types.E5
      ; (* Left *)
        Types.C4
      ; Types.D4
      ; (* Right *)
        Types.F4
      ; Types.G4
      ; Types.H4
      ]
    in
    let expected_bb =
      List.fold
        ~init:empty
        ~f:(fun acc sq -> UInt64.logor acc (square_bb sq))
        expected_attacked_squares
    in
    [%test_result: t] ~expect:expected_bb attacks
  ;;
end

let%test_unit "test_more_than_one" =
  [%test_result: bool] ~expect:false (Bitboard.more_than_one Bitboard.empty);
  [%test_result: bool]
    ~expect:false
    (Bitboard.more_than_one @@ Bitboard.square_bb Types.E4);
  [%test_result: bool] ~expect:true (Bitboard.more_than_one Bitboard.rank_1);
  [%test_result: bool] ~expect:true (Bitboard.more_than_one Bitboard.file_G)
;;

let%test_unit "test_shift" =
  [%test_result: Bitboard.t]
    ~expect:Bitboard.rank_2
    (Bitboard.shift Bitboard.rank_1 Types.NORTH);
  [%test_result: Bitboard.t]
    ~expect:Bitboard.rank_6
    (Bitboard.shift Bitboard.rank_7 Types.SOUTH);
  [%test_result: Bitboard.t]
    ~expect:Bitboard.file_B
    (Bitboard.shift Bitboard.file_A Types.EAST);
  [%test_result: Bitboard.t]
    ~expect:Bitboard.file_D
    (Bitboard.shift Bitboard.file_E Types.WEST);
  [%test_result: Bitboard.t]
    ~expect:(Bitboard.square_bb Types.E5)
    (Bitboard.shift (Bitboard.square_bb Types.D4) Types.NORTH_EAST);
  [%test_result: Bitboard.t]
    ~expect:(Bitboard.square_bb Types.F8)
    (Bitboard.shift (Bitboard.square_bb Types.G7) Types.NORTH_WEST);
  [%test_result: Bitboard.t]
    ~expect:(Bitboard.square_bb Types.H1)
    (Bitboard.shift (Bitboard.square_bb Types.G2) Types.SOUTH_EAST);
  [%test_result: Bitboard.t]
    ~expect:(Bitboard.square_bb Types.A4)
    (Bitboard.shift (Bitboard.square_bb Types.B5) Types.SOUTH_WEST)
;;

let%test_unit "test_sq_distance" =
  [%test_result: int] ~expect:4 (Bitboard.sq_distance Types.A1 Types.E4);
  [%test_result: int] ~expect:0 (Bitboard.sq_distance Types.A1 Types.A1);
  [%test_result: int] ~expect:1 (Bitboard.sq_distance Types.F6 Types.F5);
  [%test_result: int] ~expect:7 (Bitboard.sq_distance Types.H2 Types.A8)
;;

let%test_unit "test_magically_get_bishop_attack_empty_board" =
  let attacks = Bitboard.attacks_bb_occupied Types.BISHOP Types.A1 Bitboard.empty in
  let expected_attacked_squares =
    [ Types.B2; Types.C3; Types.D4; Types.E5; Types.F6; Types.G7; Types.H8 ]
  in
  let expected_bb =
    List.fold ~init:Bitboard.empty ~f:Bitboard.bb_or_sq expected_attacked_squares
  in
  [%test_result: Bitboard.t] ~expect:expected_bb attacks
;;

let%test_unit "test_magically_get_bishop_attack_self_obstruction" =
  (* Proof that self obstruction doesn't matter *)
  let attacks =
    Bitboard.attacks_bb_occupied Types.BISHOP Types.A1 (Bitboard.square_bb Types.A1)
  in
  let expected_attacked_squares =
    [ Types.B2; Types.C3; Types.D4; Types.E5; Types.F6; Types.G7; Types.H8 ]
  in
  let expected_bb =
    List.fold ~init:Bitboard.empty ~f:Bitboard.bb_or_sq expected_attacked_squares
  in
  [%test_result: Bitboard.t] ~expect:expected_bb attacks
;;

let%test_unit "test_sliding_attack_bishop_1_piece_obstructing_diagonal" =
  let occupied = Bitboard.square_bb Types.D4 in
  let attacks = Bitboard.attacks_bb_occupied Types.BISHOP Types.A1 occupied in
  let expected_attacked_squares = [ Types.B2; Types.C3; Types.D4 ] in
  let expected_bb =
    List.fold ~init:Bitboard.empty ~f:Bitboard.bb_or_sq expected_attacked_squares
  in
  [%test_result: Bitboard.t] ~expect:expected_bb attacks
;;

let%test_unit "test_sliding_attack_bishop_2_pieces_obstructing_diagonal" =
  let occupied = Bitboard.square_bb Types.D4 |> Fn.flip Bitboard.bb_or_sq Types.E5 in
  let attacks = Bitboard.attacks_bb_occupied Types.BISHOP Types.A1 occupied in
  let expected_attacked_squares = [ Types.B2; Types.C3; Types.D4 ] in
  let expected_bb =
    List.fold ~init:Bitboard.empty ~f:Bitboard.bb_or_sq expected_attacked_squares
  in
  [%test_result: Bitboard.t] ~expect:expected_bb attacks
;;

let%test_unit "test_magically_get_rook_attack_empty_board" =
  let attacks = Bitboard.attacks_bb_occupied Types.ROOK Types.E4 Bitboard.empty in
  let expected_attacked_squares =
    [ (* Down *)
      Types.E1
    ; Types.E2
    ; Types.E3
    ; (* Up *)
      Types.E5
    ; Types.E6
    ; Types.E7
    ; Types.E8
    ; (* Left *)
      Types.A4
    ; Types.B4
    ; Types.C4
    ; Types.D4
    ; (* Right *)
      Types.F4
    ; Types.G4
    ; Types.H4
    ]
  in
  let expected_bb =
    List.fold ~init:Bitboard.empty ~f:Bitboard.bb_or_sq expected_attacked_squares
  in
  [%test_result: Bitboard.t] ~expect:expected_bb attacks
;;

let%test_unit "test_magically_get_rook_attack_1_obstructing_piece" =
  let occupied =
    Bitboard.square_bb Types.E5
    |> Fn.flip Bitboard.bb_or_sq Types.E6
    |> Fn.flip Bitboard.bb_or_sq Types.C4
  in
  let attacks = Bitboard.attacks_bb_occupied Types.ROOK Types.E4 occupied in
  let expected_attacked_squares =
    [ (* Down *)
      Types.E1
    ; Types.E2
    ; Types.E3
    ; (* Up *)
      Types.E5
    ; (* Left *)
      Types.C4
    ; Types.D4
    ; (* Right *)
      Types.F4
    ; Types.G4
    ; Types.H4
    ]
  in
  let expected_bb =
    List.fold ~init:Bitboard.empty ~f:Bitboard.bb_or_sq expected_attacked_squares
  in
  [%test_result: Bitboard.t] ~expect:expected_bb attacks
;;

let%test_unit "test_get_king_attacks" =
  let attacks = Bitboard.attacks_bb_occupied Types.KING Types.E4 Bitboard.empty in
  let expected_bb =
    List.fold
      ~init:Bitboard.empty
      ~f:Bitboard.bb_or_sq
      [ Types.D3; Types.E3; Types.D5; Types.E5; Types.F5; Types.D4; Types.F4; Types.F3 ]
  in
  [%test_result: Bitboard.t] ~expect:expected_bb attacks
;;

let%test_unit "test_get_king_attacks_from_corner" =
  let attacks = Bitboard.attacks_bb_occupied Types.KING Types.H8 Bitboard.empty in
  let expected_bb =
    List.fold ~init:Bitboard.empty ~f:Bitboard.bb_or_sq [ Types.H7; Types.G8; Types.G7 ]
  in
  [%test_result: Bitboard.t] ~expect:expected_bb attacks
;;

let%test_unit "test_get_knight_attacks" =
  let attacks = Bitboard.attacks_bb_occupied Types.KNIGHT Types.E3 Bitboard.empty in
  let expected_bb =
    List.fold
      ~init:Bitboard.empty
      ~f:Bitboard.bb_or_sq
      [ Types.D5; Types.F5; Types.G4; Types.G2; Types.F1; Types.D1; Types.C2; Types.C4 ]
  in
  [%test_result: Bitboard.t] ~expect:expected_bb attacks
;;

let%test_unit "test_get_knight_from_the_rim" =
  let attacks = Bitboard.attacks_bb_occupied Types.KNIGHT Types.A3 Bitboard.empty in
  let expected_bb =
    List.fold
      ~init:Bitboard.empty
      ~f:Bitboard.bb_or_sq
      [ Types.B5; Types.C4; Types.C2; Types.B1 ]
  in
  [%test_result: Bitboard.t] ~expect:expected_bb attacks
;;

let%test_unit "test_line_bb_diagonal" =
  let expected =
    List.fold
      ~init:Bitboard.empty
      ~f:Bitboard.bb_or_sq
      [ Types.A2; Types.B3; Types.C4; Types.D5; Types.E6; Types.F7; Types.G8 ]
  in
  [%test_result: Bitboard.t] ~expect:expected (Bitboard.line_bb Types.C4 Types.F7)
;;

let%test_unit "test_line_bb_vertical" =
  let expected =
    List.fold
      ~init:Bitboard.empty
      ~f:Bitboard.bb_or_sq
      [ Types.A1; Types.A2; Types.A3; Types.A4; Types.A5; Types.A6; Types.A7; Types.A8 ]
  in
  [%test_result: Bitboard.t] ~expect:expected (Bitboard.line_bb Types.A4 Types.A2)
;;

let%test_unit "test_between_bb_diagonal" =
  let expected =
    List.fold ~init:Bitboard.empty ~f:Bitboard.bb_or_sq [ Types.D5; Types.E6; Types.F7 ]
  in
  [%test_result: Bitboard.t] ~expect:expected (Bitboard.between_bb Types.C4 Types.F7)
;;

let%test_unit "test_between_bb_horizontal" =
  let expected =
    List.fold
      ~init:Bitboard.empty
      ~f:Bitboard.bb_or_sq
      [ Types.F2; Types.E2; Types.D2; Types.C2; Types.B2; Types.A2 ]
  in
  [%test_result: Bitboard.t] ~expect:expected (Bitboard.between_bb Types.G2 Types.A2)
;;

let%test_unit "test_aligned" =
  [%test_result: bool] ~expect:true (Bitboard.aligned Types.E4 Types.F5 Types.G6);
  [%test_result: bool] ~expect:true (Bitboard.aligned Types.E4 Types.E5 Types.E6);
  [%test_result: bool] ~expect:false (Bitboard.aligned Types.E4 Types.E5 Types.F6)
;;

let%test_unit "test_bb_to_square" =
  List.iter
    ~f:(fun sq ->
      [%test_result: Types.square]
        ~expect:sq
        (Bitboard.square_bb sq |> Bitboard.bb_to_square))
    Types.all_squares
;;
