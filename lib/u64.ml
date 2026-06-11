(* Drop-in replacement for [Unsigned.UInt64] backed by [int64].

   The integers library implements every UInt64 operation - including logand,
   logor and the shifts on the hot bitboard paths - as a C stub that allocates
   a fresh custom block for its result. ocamlopt compiles the corresponding
   [Int64] operations to single instructions and keeps intermediates unboxed,
   so this module exposes the subset of the UInt64 API the engine uses with
   bit-pattern-identical semantics:

   - [shift_right] is logical (unsigned semantics), never arithmetic.
   - [compare] uses the unsigned order.
   - [to_string]/[of_string] read and print the unsigned interpretation
     (decimal magic constants in this codebase exceed [Int64.max_int]).

   Everything is qualified through [Stdlib] so the module behaves the same
   whether or not [Base] is open at the use site. *)

module UInt64 = struct
  module I = Stdlib.Int64

  type t = int64

  let zero = 0L
  let one = 1L
  let max_int = -1L
  let add = I.add
  let sub = I.sub
  let mul = I.mul
  let succ = I.succ
  let pred = I.pred
  let logand = I.logand
  let logor = I.logor
  let logxor = I.logxor
  let lognot = I.lognot
  let shift_left = I.shift_left
  let shift_right = I.shift_right_logical
  let equal = I.equal
  let compare = I.unsigned_compare
  let of_int = I.of_int
  let to_int = I.to_int
  let of_int64 (x : int64) : t = x
  let to_int64 (x : t) : int64 = x
  let to_string t = Stdlib.Printf.sprintf "%Lu" t

  let of_string s =
    if
      Stdlib.String.length s > 1
      && Stdlib.Char.equal s.[0] '0'
      && (Stdlib.Char.equal s.[1] 'x' || Stdlib.Char.equal s.[1] 'X')
    then I.of_string s
    else I.of_string ("0u" ^ s)
  ;;

  module Infix = struct
    let ( + ) = I.add
    let ( - ) = I.sub
    let ( * ) = I.mul
    let ( land ) = I.logand
    let ( lor ) = I.logor
    let ( lxor ) = I.logxor
    let ( lsl ) = I.shift_left
    let ( lsr ) = I.shift_right_logical
  end
end
