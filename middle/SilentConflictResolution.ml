open MiddleAPI

module Run
(A : LR1_AUTOMATON)
(Precedence : sig
  open A.Lr0.G

  (**A choice indicates how a shift/reduce conflict should be resolved. *)
  type choice =
    | ChooseShift
    | ChooseReduce
    | ChooseNeither
    | DontKnow

  (**[shift_reduce t prod] determines how a shift/reduce conflict between the
     terminal symbol [t] and the production [prod] should be resolved. *)
  val shift_reduce: Terminal.t -> Production.index -> choice

end)
= struct
module Lr0 = A.Lr0
open Lr0.G

(* -------------------------------------------------------------------------- *)

(* We refer to a node of the automaton [A] as a raw node. *)

(* In the following, we perform a depth-first traversal of the automaton [A].
   As we go, we perform silent conflict resolution (which can remove some
   transitions and therefore make some nodes unreachable) and we re-number
   the reachable nodes. *)

(* We define our own type [node] to be the number of a reachable node in this
   new numbering system. *)

(* A conflict that is not resolved by silent conflict resolution is severe. *)

type node =
  int

(* -------------------------------------------------------------------------- *)

(* All of the following mutable state is modified or initialized during the
   depth-first traversal below. *)

(* All of the arrays below are indexed by raw node indices, that is, indices
   that make sense with respect to the automaton [A]. *)

module M =
  DFS.MarkArray(A)

(* The array [_number] is initialized during the traversal.
   We assign new consecutive numbers to the reachable nodes. *)

let unreachable =
  -1

let number : node array =
  Array.make A.n unreachable

let transport (raw_node : A.node) : node =
  number.(A.encode raw_node)

(* The array [transitions] is initialized here with the data supplied by the
   function [A.transitions]. During the traversal, some transitions are
   removed, because of conflict resolution. *)

let transitions : A.node SymbolMap.t array =
  Array.init A.n @@ fun i ->
  A.transitions (A.decode i)

(* The array [reductions] stores the reductions at a node. *)

let reductions : Reductions.t array =
  Array.make A.n TerminalMap.empty

(* The array [severe_conflict_symbols] records the terminal symbols for which
   a conflict still exists after silent conflict resolution has been
   performed. *)

let severe_conflict_symbols : TerminalSet.t array =
  Array.make A.n TerminalSet.empty

(* The array [eos_conflict] records an optional end-of-stream conflicts in
   each state. When present, [prods] are the productions that can be reduced
   on [#], and [ts] is the set of terminal symbols on which other actions
   are permitted. *)

let eos_conflict : (Production.t list * TerminalSet.t) option array =
  Array.make A.n None

(* (New as of 2012/01/23.) The array [forbid_default_reduction] records
   whether a shift/reduce conflict was solved in favor of neither action
   (%nonassoc). This is later used to forbid a default reduction. *)

let forbid_default_reduction : bool array =
  Array.make A.n false

(* A count of all reachable nodes in the new automaton. *)

let n =
  ref 0

(* A list of all nodes with severe conflicts. *)

let severe_conflict_nodes : node list ref =
  ref []

(* Counts of nodes with severe shift/reduce and severe reduce/reduce
   conflicts. *)

let shift_reduce =
  ref 0

let reduce_reduce =
  ref 0

(* Count of the shift/reduce conflicts that could be silently resolved. *)

let silently_solved =
  ref 0

(* -------------------------------------------------------------------------- *)

(* A view of the automaton [A] as a graph. *)

(* This view relies on the [transitions] array, as opposed to the function
   [A.transitions]. This means that, once an edge has been removed, it can
   no longer be followed. *)

module RawForwardEdges = struct
  type node = A.node
  type label = Symbol.t
  let foreach_outgoing_edge node yield =
    let i = A.encode node in
    SymbolMap.iter yield transitions.(i)
  let foreach_root yield =
    ProductionMap.iter (fun _prod node -> yield node) A.entry
end

(* -------------------------------------------------------------------------- *)

(* [resolve] is invoked at the raw node [i] when there are reduction actions
   described by [t] and [prods] and there is an outgoing transition along [t].
   Therefore, there is at least a shift/reduce conflict. *)

(* [resolve] sets the flags [hasSR] and [hasRR] to record severe shift/reduce
   and reduce/reduce conflicts. Furthermore, it returns a Boolean flag, which
   indicates whether the reduction actions should be kept ([true]) or silently
   removed ([false]). *)

let rec resolve hasSR hasRR i t prods : bool =
  assert (not (Terminal.equal t Terminal.sharp));
  assert (prods <> []);
  if List.length prods = 1 then
    let prod = List.hd prods in
    resolve1 hasSR hasRR i t prod
  else
    resolve2 hasSR hasRR i t prods

(* [resolve1] deals with the case where only one reduction action exists.
   Therefore, this is a single shift/reduce conflict. If the user-provided
   precedence declarations tell us how to solve it, follow them and modify
   the automaton by removing a transition, a reduction, or both. *)

and resolve1 hasSR _hasRR i t prod : bool =
  let open Precedence in
  match shift_reduce t prod with
  | ChooseShift ->
      prefer_shift i t [prod]
  | ChooseReduce ->
      prefer_reduce i t
  | ChooseNeither ->
      prefer_neither i t [prod]
  | DontKnow ->
      hasSR := true;
      record_severe_conflict i t

(* [resolve2] deals with the case where two or more reduction actions exist.
   This is a shift/reduce/reduce conflict. If the priorities are such that
   each individual shift/reduce conflict is solved in favor of shifting or in
   favor of neither, then solve the entire composite conflict in the same way.
   Otherwise, report the conflict. *)

and resolve2 hasSR hasRR i t prods =
  let open Precedence in
  let choices = List.map (shift_reduce t) prods in
  if List.for_all ((=) ChooseShift) choices then
    prefer_shift i t prods
  else if List.for_all ((=) ChooseNeither) choices then
    prefer_neither i t prods
  else begin
    hasSR := true;
    hasRR := true;
    record_severe_conflict i t
  end

(* Auxiliary functions. *)

and prefer_shift _i _t prods : bool =
  silently_solved := !silently_solved + List.length prods;
  false (* remove these reductions *)

and prefer_reduce i t : bool =
  (* Keep the reduce action and remove the transition. The automaton is
     modified in place. This can have the subtle effect of making some
     nodes unreachable. Any conflicts in these nodes are then ignored. *)
  incr silently_solved;
  transitions.(i) <- SymbolMap.remove (Symbol.T t) transitions.(i);
  true (* keep these reductions *)

and prefer_neither i t prods : bool =
  silently_solved := !silently_solved + List.length prods;
  transitions.(i) <- SymbolMap.remove (Symbol.T t) transitions.(i);
  forbid_default_reduction.(i) <- true;
  false (* remove these reductions *)

and record_severe_conflict i t : bool =
  (* Record the existence of a severe shift/reduce conflict. *)
  severe_conflict_symbols.(i) <- TerminalSet.add t severe_conflict_symbols.(i);
  true (* keep these reductions *)

(* -------------------------------------------------------------------------- *)

(* [discover] is invoked during the traversal when a node is discovered. *)

(* It is in charge of detecting and resolving conflicts at this node. *)

let discover (raw_node : A.node) =
  let i = A.encode raw_node in

  (* Number this node. *)

  let node : node = MInt.postincrement n in
  number.(i) <- node;

  (* Detect conflicts. We iterate over the table of all potential reductions.
     The code is written so as to detect multi-way shift/reduce/reduce
     conflicts. We solve conflicts, when unambiguously allowed by the
     user-provided precedence declarations, by removing certain transitions
     and reductions. *)

  let hasSR = ref false
  and hasRR = ref false in

  let retained =
    A.reductions raw_node |> TerminalMap.filter @@ fun t prods ->
    if SymbolMap.mem (Symbol.T t) transitions.(i) then
      (* There is a transition and at least one reduction. *)
      resolve hasSR hasRR i t prods
    else if List.length prods >= 2 then begin
      (* There is no transition and there are multiple reductions. This is a pure
         reduce/reduce conflict. *)
      hasRR := true;
      record_severe_conflict i t
    end
    else
      (* There is no transition and there is one reduction. No conflict. *)
      true (* keep this reduction *)
  in
  reductions.(i) <- retained;

  (* Detect end-of-stream conflicts at this node. If it has both a reduce
     action at [#] and some other (shift or reduce) action, this is an
     end-of-stream conflict. *)

  let transitions = transitions.(i)
  and reductions = reductions.(i) in

  if Reductions.has_eos_conflict transitions reductions then begin
    (* Conceptually suppress the reduce action at [#]. *)
      let prods, reductions =
        TerminalMap.find_and_remove Terminal.sharp reductions in
    (* Compute the tokens involved in the transitions and remaining
       reductions. *)
    let toks =
      TerminalSet.union
        (SymbolMap.terminals transitions)
        (TerminalMap.domain reductions)
    in
    (* Record this end-of-stream conflict. *)
    eos_conflict.(i) <- Some (prods, toks)
  end;

  (* Record statistics about conflicts. *)

  if not (TerminalSet.is_empty severe_conflict_symbols.(i)) then begin
    severe_conflict_nodes := node :: !severe_conflict_nodes;
    if !hasSR then
      incr shift_reduce;
    if !hasRR then
      incr reduce_reduce
  end

(* -------------------------------------------------------------------------- *)

end (* Run *)
