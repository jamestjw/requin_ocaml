open Position
open Types

type phase = int

let phase_endgame = 0
let phase_midgame = 128

(* Values modified by Joona Kiiski *)
let midgame_limit = 15581
let endgame_limit = 3998

type scale_factor = int

let scale_factor_draw = 0
let scale_factor_normal = 64
let scale_factor_max = 128
let scale_factor_none = 255

(* game_phase() calculates the phase given the current
   position. *)

let game_phase pos =
  let npm =
    Position.non_pawn_material_for_colour pos Types.WHITE
    + Position.non_pawn_material_for_colour pos Types.BLACK
  in
  if npm >= midgame_limit
  then phase_midgame
  else if npm <= endgame_limit
  then phase_endgame
  else (npm - endgame_limit) * 128 / (midgame_limit - endgame_limit)
;;
