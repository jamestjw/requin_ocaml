open Base
open Types
open Unsigned

(* Inspired by https://github.com/kobolabs/stockfish/blob/master/tt.h *)

let cluster_size = 4
let num_entries = 1572864

type bound =
  | BOUND_NONE
  | BOUND_UPPER
  | BOUND_LOWER
  | BOUND_EXACT
[@@deriving eq]

(* Does b1 contain b2? i.e. bool(b1 & b2) *)
let bound_contains b1 b2 =
  match b1, b2 with
  | BOUND_NONE, BOUND_NONE -> true
  | _, BOUND_NONE | BOUND_NONE, _ -> false
  | BOUND_EXACT, _ | _, BOUND_EXACT -> true
  | _ -> equal_bound b1 b2
;;

type entry =
  { key : UInt32.t
  ; depth : int
  ; is_pv : bool
  ; bound : bound
  ; move : Types.move
  ; value : Types.value
  ; eval_value : Types.value
  ; generation : int
  }

let empty_entry =
  { key = UInt32.zero
  ; depth = 0
  ; is_pv = false
  ; bound = BOUND_NONE
  ; move = Types.none_move
  ; value = Types.value_none
  ; eval_value = Types.value_none
  ; generation = 0
  }
;;

type cluster = entry array

type t =
  { data : cluster array
  ; size : int
  ; generation : int
  }

(* TODO: Allow custom sizes *)
let mk () =
  let num_clusters = num_entries / cluster_size in
  let data = Array.make_matrix ~dimx:num_clusters ~dimy:cluster_size empty_entry in
  { data; size = num_clusters; generation = 0 }
;;

let cluster_idx { size; _ } key =
  (* Only take the lower 32 bits *)
  UInt64.to_uint32 key |> UInt32.(logand @@ of_int (size - 1)) |> UInt32.to_int
;;

(* Overwrites the entire transposition table with zeroes *)
let clear { data; _ } =
  (* TODO: Should this zero out the `generation` field too? *)
  Array.iter data ~f:(fun c -> Array.fill c ~pos:0 ~len:cluster_size empty_entry)
;;

let probe ({ data; _ } as tt) key =
  let entry_key = UInt64.to_uint32 key in
  let idx = cluster_idx tt key in
  Array.find (Array.get data idx) ~f:(fun entry -> UInt32.equal entry_key entry.key)
;;

(* Stores data to the TT and returns the entry that was saved *)
let store
      ({ data; size; generation = tt_generation; _ } as tt)
      ~key
      ~m
      ~depth
      ~is_pv
      ~bound
      ~value
      ~eval_value
  =
  let entry_key = UInt64.to_uint32 key in
  let cluster = Array.get data @@ cluster_idx tt key in
  let rec find_set_idx i (m, best) =
    if i < size
    then (
      let { key; move = tt_move; generation = generation'; bound; depth; _ } =
        Array.get cluster i
      in
      (* Entry is either empty, or we found an old entry that we should
         update *)
      if UInt32.(equal key zero || equal key entry_key)
      then
        (* If the new move is none, then we preserve the old move *)
        if Types.equal_move m Types.none_move then tt_move, i else m, i
      else (
        (* Implement replace strategy *)
        let replace = Array.get cluster best in
        (* If the previous candidate is from an older generation, then it is
           better than this one *)
        let c1 = if replace.generation = tt_generation then 2 else 0 in
        (* If this entry is from the current generation, then we don't really
           want to replace it. If it has an exact bound, then it is also more
           likely to be valuable and hence undesirable to replace. *)
        let c2 =
          if generation' = tt_generation || equal_bound bound BOUND_EXACT then -2 else 0
        in
        (* If the current entry has been searched to a lower depth, then we
           should replace it. *)
        let c3 = if depth < replace.depth then 1 else 0 in
        let new_acc = if c1 + c2 + c3 > 0 then m, i else m, best in
        find_set_idx (i + 1) new_acc))
    else m, best
  in
  let m, set_idx = find_set_idx 0 (m, 0) in
  let tte =
    { key = entry_key
    ; generation = tt_generation
    ; move = m
    ; depth
    ; is_pv
    ; bound
    ; value
    ; eval_value
    }
  in
  Array.set cluster set_idx tte;
  tte
;;

(*
   * `new_search` is called at the beginning of every new search.
 * It increments the `generation` variable, which is used to
 * distinguish transposition table entries from previous searches
 * from entries from the current search.
*)
let new_search ({ generation; _ } as tt) = { tt with generation = generation + 1 }
