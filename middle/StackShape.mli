(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open MiddleAPI

module Make
(Lr1 : LR1)
(X : sig

  (**If [represent_states] is [true] then every stack cell is forced to
     contain a state. If it is [false] then an analysis is performed so as
     determine which stack cells do and do not need to hold a state. *)
  val represent_states: bool

  (**If [represent_positions] is [true] then every stack cell is forced to
     contain a start position and an end position. If it is [false] then an
     analysis is performed so as determine which stack cells do and do not
     need to hold a start position and an end position. *)
  val represent_positions: bool

  (**If [represent_values] is [true] then every stack cell is forced to
     contain a semantic value. If it is [false] then a stack cell that
     corresponds to a terminal symbol whose OCaml type is [unit] does not
     contain a semantic value. *)
  val represent_values: bool

end)
(E : sig

  (**[log format ...] emits an information message. *)
  val log: ('a, out_channel, unit, unit) format4 -> 'a

end)
: sig

  include KINDERGARTEN
    with type symbol := Lr1.Lr0.G.Symbol.t
     and type node := Lr1.node
     and type nodes := Lr1.NodeSet.t

  module Short : SHAPE
    with module G = Lr1.Lr0.G
     and type node := Lr1.node
     and type shape := shape

  module Long : SHAPE
    with module G = Lr1.Lr0.G
     and type node := Lr1.node
     and type shape := shape

end
