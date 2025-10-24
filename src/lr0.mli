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

  (**[compare] is a total order on states. *)
  val compare: t -> t -> int

  (**[export s] is the concrete LR(1) state that corresponds to the state
     [s]. *)
  val export: t -> ConcreteLR1State.t

  (**[core s] is the core of the state [s], that is, the underlying
     LR(0) state that is obtained by erasing the lookahead sets. *)
  val core: t -> node

  (**If [node] is a start node of the LR(0) automaton then [start node]
     is the corresponding LR(1) start state. *)
  val start: node -> t

  (**[transitions s] returns the outgoing (shift and goto) transitions of the
     state [s], in the form of a map of symbols (which serve as edge labels)
     to successor states. *)
  val transitions: t -> t SymbolMap.t

  (**[transition symbol s] return the successor of the state [s] along the
     transition labeled [symbol]. This transition must exist. *)
  val transition: Symbol.t -> t -> t

  (**[reductions s] is a reduction table for the state [s]. *)
  val reductions: t -> Reductions.t

  (**[reverse_reductions s] is a reverse reduction table for the state [s]. *)
  val reverse_reductions: t -> Reductions.reverse

  (**[equal s1 s2] determines whether the states [s1] and [s2] are equal, that
     is, whether their lookahead sets are pointwise equal. This test is
     permitted only if [s1] and [s2] have the same core. *)
  val equal: t -> t -> bool

  (**[subset s1 s2] determines whether the states [s1] and [s2] are in the
     subset relation, that is, whether their lookahead sets are pointwise in the
     subset relation. This test is permitted only if [s1] and [s2] have the same
     core. *)
  val subset: t -> t -> bool

  (**[compatible s1 s2] determines whether the states [s1] and [s2] are
     compatible according to a slightly modified version of Pager's weak
     compatibility criterion. This test is permitted only if [s1] and [s2] have
     the same core. *)
  val compatible: t -> t -> bool

  (**[eos_compatible s1 s2] determines whether the states [s1] and [s2] can be
     merged without creating an end-of-stream conflict. This test is permitted
     only if [s1] and [s2] have the same core. *)
  val eos_compatible: t -> t -> bool

  (**[error_compatible s1 s2] determines whether the states [s1] and [s2] can be
     merged without creating spurious reductions on [error]. This test is
     permitted only if [s1] and [s2] have the same core. *)
  val error_compatible: t -> t -> bool

  (**[union s1 s2] computes the union of the states [s1] and [s2]. This is
     permitted only if [s1] and [s2] have the same core.The new state is
     obtained by pointwise union of the lookahead sets. If [s1] is a subset of
     [s2], then [union s1 s2] is physically equal to [s2]. *)
  val union: t -> t -> t

  (**[restrict ts s] restricts the state [s] to the set of terminal symbols
     [ts]. Every lookahead set in [s] is intersected with the set [ts]. *)
  val restrict: TerminalSet.t -> t -> t

  (**[print leading s] is a string representation of the state [s].
     The string [leading] is inserted at the beginning of every line. *)
  val print: string -> t -> string

end (* AbstractLR1State *)
