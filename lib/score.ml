(* Midgame and endgame values *)
type t =
  { mg : int
  ; eg : int
  }

let mk mg eg = { mg; eg }
let neg { mg; eg } = { mg = -mg; eg = -eg }
let zero = mk 0 0
let add { mg = mg1; eg = eg1 } { mg = mg2; eg = eg2 } = { mg = mg1 + mg2; eg = eg1 + eg2 }
let sub { mg = mg1; eg = eg1 } { mg = mg2; eg = eg2 } = { mg = mg1 - mg2; eg = eg1 - eg2 }

module Infix = struct
  let ( + ) = add
  let ( - ) = sub
end
