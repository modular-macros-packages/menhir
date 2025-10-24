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

(**[outgoing_edges node] returns the outgoing (shift and goto) edges of the
   node [node], in the form of a map of symbols (which serve as edge labels)
   to successor nodes. *)
val outgoing_edges: node -> node SymbolMap.t

(**[foreach_outgoing_edge node] enumerates the outgoing edges of the node
   [node]. *)
val foreach_outgoing_edge: node -> (Symbol.t -> node -> unit) -> unit

(**[outgoing_symbols node] constructs a map whose domain is the outgoing
   symbols of the node [node]. *)
val outgoing_symbols: node -> (Symbol.t -> 'a) -> 'a SymbolMap.t

(* -------------------------------------------------------------------------- *)

(**This module offers a concrete representation of LR(1) states. *)
module ConcreteLR1State : sig

  (**An LR(1) state is a mapping of LR(0) items to lookahead sets, that is,
     sets of terminal symbols. This is a concrete representation: it is
     simple but heavyweight. *)
  type t =
    TerminalSet.t Item.Map.t

  (**[closure s] is the closure of the LR(1) state [s]. *)
  val closure: t -> t

  (**[print leading s] is a string representation of the LR(1) state [s].
     The string [leading] is inserted at the beginning of every line. *)
  val print: string -> t -> string

end

(* -------------------------------------------------------------------------- *)

(**This module offers an abstract representation of LR(1) states, which is
   particularly useful to construct of various kinds of LR(1) automata. *)
module AbstractLR1State : sig

  (**An LR(1) state is internally represented as a pair of an LR(0) state
     number and an array of concrete lookahead sets, whose length depends on
     the LR(0) state. This representation is made available as an abstract
     type. It is equipped with a series of efficient operations. *)
  type t

  (**[compare] is a total order on LR(1) states. *)
  val compare: t -> t -> int

  (**[export s] is the concrete LR(1) state that corresponds to the abstract
     LR(1) state [s]. *)
  val export: t -> ConcreteLR1State.t

  (**[core s] is the core of the LR(1) state [s], that is, the underlying
     LR(0) state that is obtained by erasing the lookahead sets. *)
  val core: t -> node

  (**If [node] is a start node of the LR(0) automaton then [start node]
     is the corresponding LR(1) start state. *)
  val start: node -> t

  (**[transitions s] returns the outgoing (shift and goto) transitions of the
     state [s], in the form of a map of symbols (which serve as edge labels)
     to successor states. *)
  val transitions: t -> t SymbolMap.t

  val transition: Symbol.t -> t -> t

  (* [transition_tokens transitions] returns the set of tokens (terminal symbols)
     that are labels of outgoing transitions in the table [transitions]. *)
  val transition_tokens: 'target SymbolMap.t -> TerminalSet.t

  (* Information about the reductions at a state. *)
  val reductions: t -> (TerminalSet.t * Production.t) list

  (* A table of the reductions at a state. *)
  val reductions_table: t -> Reductions.t

  (* Equality of states. The two states must have the same core. Then,
     they are equal if and only if their lookahead sets are pointwise
     equal. *)

  val equal: t -> t -> bool

  (* Subsumption between states. The two states must have the same
     core. Then, one subsumes the other if and only if their lookahead
     sets are (pointwise) in the subset relation. *)

  val subsume: t -> t -> bool

  (* A slightly modified version of Pager's weak compatibility
     criterion. The two states must have the same core. *)

  val compatible: t -> t -> bool

  (* This function determines whether two (core-equivalent) states can
     be merged without creating an end-of-stream conflict. *)

  val eos_compatible: t -> t -> bool

  (* This function determines whether two (core-equivalent) states can
     be merged without creating spurious reductions on the [error]
     token. *)

  val error_compatible: t -> t -> bool

  (* Union of two states. The two states must have the same core. The
     new state is obtained by pointwise union of the lookahead sets. *)

  (* If [s'] is a subset of [s], then [union s' s] is physically equal
     to [s]. *)

  val union: t -> t -> t

  (* Restriction of a state to a set of tokens of interest. Every
     lookahead set is intersected with that set. *)

  val restrict: TerminalSet.t -> t -> t

  (* The following functions display: 1- a concrete state; 2- a state
     (only the kernel, not the closure);
     The first parameter is a fixed string that is added at the
     beginning of every line. *)

  val print:          string ->         t -> string

end (* Help *)
