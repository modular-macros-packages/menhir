(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module computes and publishes information about the shape and
   content of the stack in an LR(1) automaton.

   Using [StackSymbols] and [StackStates], it determines the shape and
   content of the stack in several situations: 1- when a state is about to
   be entered, 2- when a production is about to be reduced, and 3- when a
   goto transition is about to be taken.

   Furthermore, it determines which states should be represented (that is,
   which states need to physically exist on the stack at runtime) and
   which symbols need to keep track of their start or end positions (that
   is, which positions need to physically exist on the stack at runtime).

   This information computed is used in the code back-end and in the Rocq
   back-end. It is not used in the table back-end. *)

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

  (**The types [shape], [cell], and their operations. *)
  include KINDERGARTEN
    with type symbol := Lr1.Lr0.G.Symbol.t
     and type node := Lr1.node
     and type nodes := Lr1.NodeSet.t

  (**[print_represented c] prints which states are represented on the
     output channel [c]. *)
  val print_represented: out_channel -> unit

  (**In the "short invariant", the length of the known shape of the stack
     at a node [node] is the maximum position of the bullet in the items
     of this node. *)
  module Short : sig

    include SHAPE
      with module G = Lr1.Lr0.G
       and type node := Lr1.node
       and type shape := shape

    (**[print_stack_states c] prints the results of the stack states
       analysis on the output channel [c]. *)
    val print_stack_states: out_channel -> unit

  end

  (**In the "long invariant", the length of the known shape of the stack
     can be longer than in the short invariant. It is obtained via a data
     flow analysis of the LR(1) automaton. *)
  module Long : sig

    include SHAPE
      with module G = Lr1.Lr0.G
       and type node := Lr1.node
       and type shape := shape

    (**[print_stack_states c] prints the results of the stack states
       analysis on the output channel [c]. *)
    val print_stack_states: out_channel -> unit

  end

end
