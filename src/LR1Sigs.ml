(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Grammar

(* The output signature of several LR(1) automaton construction algorithms. *)

module type LR1_AUTOMATON = sig

  (**A state of the LR(1) automaton is known as a node. *)
  type node

  (**[n] is the number of nodes in the LR(1) automaton. *)
  val n: int

  (* Nodes are numbered from 0 to [n-1]. *)

  (**[number] converts a node to an integer in the range [\[0, n)]. *)
  val number: node -> int

  (**[node] converts an integer in the range [\[0, n)] to a node. This
     function is {b unsafe} and should be used as little as possible. *)
  val node: int -> node

  (**[entry] is a map of each start production to the corresponding start node
     in the LR(1) automaton. *)
  val entry : node ProductionMap.t

    (**[transitions node] returns the outgoing (shift and goto) transitions of
       the node [node], in the form a map of symbols to successor nodes.
       (Note to implementors of the signature [LR1_AUTOMATON]: there is no
       need to memoize this function; this is done a posteriori, in [Lr1].) *)
  val transitions: node -> node SymbolMap.t

  (**[state node] is the abstract LR(1) state that corresponds to the node
     [node]. *)
  val state: node -> Lr0.ALR1.t

end
