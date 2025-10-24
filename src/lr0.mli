(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module builds the LR(0) automaton associated with a grammar
   and provides access to this automaton. Furthermore, it provides
   facilities for the efficient construction of LR(1) automata. *)

open Grammar

(**A state of the LR(0) automaton is known as a node.
   A node is an integer in the interval [\[0, n)]. *)
type node =
  int
  (* We would ideally make [node] an abstract type, but this would make
     our life unduly hard, as we like to use arrays indexed with nodes,
     apply [Fix.DataFlow.ForIntSegment] to the type [node], etc. *)

(**[n] is the number of nodes in the LR(0) automaton. *)
val n: int

(**[entry] is a map of each start production to the corresponding start node
   in the LR(0) automaton. *)
val entry: node ProductionMap.t

(**Each node in the LR(0) represents a set of LR(0) items.
   [items node] returns this set. This set is not closed. *)
val items: node -> Item.Set.t

(**All of the edges that enter a node carry the same symbol. If a node has at
   least one incoming edge, then this common symbol is known as the incoming
   symbol of this node. A node that has no incoming edges has no incoming
   symbol. A node has no incoming symbol if and only if it is a start node.
   [incoming_symbol node] is the incoming symbol of the node [node]. *)
val incoming_symbol: node -> Symbol.t option

val foreach_outgoing_edge: node -> (Symbol.t -> node -> unit) -> unit

(* The outgoing edges of a node. *)
val outgoing_edges: node -> node SymbolMap.t
val outgoing_symbols: node -> (Symbol.t -> 'a) -> 'a SymbolMap.t

(* ------------------------------------------------------------------------ *)
(* Help for building the LR(1) automaton. *)

(* An LR(1) state is internally represented as a pair of an LR(0)
   state number and an array of concrete lookahead sets (whose length
   depends on the LR(0) state). *)

type lr1state

(* A view of the type [lr1state] as an ordered type. *)

module Lr1StateAsOrderedType : sig
  type t = lr1state
  val compare: t -> t -> int
end

(* An encoded LR(1) state can be turned into a concrete representation,
   that is, a mapping of items to concrete lookahead sets. *)

type concretelr1state =
    TerminalSet.t Item.Map.t

val export: lr1state -> concretelr1state

(* One can take the closure of a concrete LR(1) state. *)

val closure: concretelr1state -> concretelr1state

(* The core of an LR(1) state is the underlying LR(0) state. *)

val core: lr1state -> node

(* One can create an LR(1) start state out of an LR(0) start
   node. *)

val start: node -> lr1state

(* Information about the transitions at a state. *)

val transitions: lr1state -> lr1state SymbolMap.t
val transition: Symbol.t -> lr1state -> lr1state

(* [transition_tokens transitions] returns the set of tokens (terminal symbols)
   that are labels of outgoing transitions in the table [transitions]. *)

val transition_tokens: 'target SymbolMap.t -> TerminalSet.t

(* Information about the reductions at a state. *)

(* See also [reductions_table] further on. *)

val reductions: lr1state -> (TerminalSet.t * Production.t) list

(* Equality of states. The two states must have the same core. Then,
   they are equal if and only if their lookahead sets are pointwise
   equal. *)

val equal: lr1state -> lr1state -> bool

(* Subsumption between states. The two states must have the same
   core. Then, one subsumes the other if and only if their lookahead
   sets are (pointwise) in the subset relation. *)

val subsume: lr1state -> lr1state -> bool

(* A slightly modified version of Pager's weak compatibility
   criterion. The two states must have the same core. *)

val compatible: lr1state -> lr1state -> bool

(* This function determines whether two (core-equivalent) states can
   be merged without creating an end-of-stream conflict. *)

val eos_compatible: lr1state -> lr1state -> bool

(* This function determines whether two (core-equivalent) states can
   be merged without creating spurious reductions on the [error]
   token. *)

val error_compatible: lr1state -> lr1state -> bool

(* Union of two states. The two states must have the same core. The
   new state is obtained by pointwise union of the lookahead sets. *)

(* If [s'] is a subset of [s], then [union s' s] is physically equal
   to [s]. *)

val union: lr1state -> lr1state -> lr1state

(* Restriction of a state to a set of tokens of interest. Every
   lookahead set is intersected with that set. *)

val restrict: TerminalSet.t -> lr1state -> lr1state

(* The following functions display: 1- a concrete state; 2- a state
   (only the kernel, not the closure); 3- the closure of a state.
   The first parameter is a fixed string that is added at the
   beginning of every line. *)

val print_concrete: string -> concretelr1state -> string
val print:          string ->         lr1state -> string

(* A table of the reductions at a state. *)

val reductions_table: lr1state -> Reductions.t
