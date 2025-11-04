(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module performs a static analysis of the LR(1) automaton in order to
   determine which states might possibly be held in the known suffix of the
   stack at every state. *)

open MiddleAPI

(**The functor [Run] performs a simple data flow analysis to determine
   which states may appear in each stack cell. *)
module Run
(Lr1 : LR1)
(S : STACK_HEIGHTS with module G = Lr1.Lr0.G and type node := Lr1.node)
   : STACK_STATES  with module G = Lr1.Lr0.G and type node := Lr1.node and type property = Lr1.NodeSet.t array
