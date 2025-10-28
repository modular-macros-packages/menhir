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

open Grammar

(* -------------------------------------------------------------------------- *)

(* At log level 1, run the SLR(1) test. *)

let () =
  Error.logG 1 @@ fun f ->
  let module SLR = SLR.Make(Lr0) in
  let count = SLR.count_violations() in
  if count = 0 then
    Printf.fprintf f "The grammar is SLR(1).\n"
  else
    Printf.fprintf f "The grammar is not SLR(1) -- %d states have a conflict.\n" count

(* -------------------------------------------------------------------------- *)

(* Select a construction algorithm based on the command-line settings. *)

module type ALGORITHM =
  LR1_ALGORITHM with module Lr0 = Lr0

let algo, mode =
  Settings.(match construction_mode with
  | ModeCanonical ->
      (module LR1Canonical.Make(Lr0) : ALGORITHM),
      "canonical"
  | ModeInclusionOnly ->
      let module X = struct let mode = `InclusionOnly end in
      (module LR1Pager.Make(Lr0)(X) : ALGORITHM),
      "no-pager"
  | ModePager ->
      let module X = struct let mode = `Pager end in
      (module LR1Pager.Make(Lr0)(X) : ALGORITHM),
      "pager"
  | ModeLALR ->
      (module LALR.Make(Lr0) : ALGORITHM),
      "lalr"
  )

module Algorithm =
  (val algo : ALGORITHM)

let () =
  Error.logA 1 (fun f ->
    Printf.fprintf f
      "The construction mode is %s.\n"
      mode
  )

(* -------------------------------------------------------------------------- *)

(* Run the construction algorithm. *)

module Raw : (LR1_AUTOMATON with module Lr0 = Lr0) = struct

  include Algorithm.Run()

  type t = node

  (* Expose an ideal reduction table at each node. This table is obtained just
     by inspection of the LR(1) state [state i]. It does not depend on the
     construction method. *)

  let reductions (i : node) : Reductions.t =
    Lr0.ALR1.reductions (state i)

  (* Allow default reductions everywhere. *)

  let forbid_default_reduction (_i : node) : bool =
    false

end

let () =
  Error.logA 1 (fun f ->
    Printf.fprintf f "Built an LR(1) automaton with %d states.\n" Raw.n
  )

(* -------------------------------------------------------------------------- *)

(* In the following, we perform a depth-first traversal of the raw automaton
   that was built above. As we go, we perform silent conflict resolution
   (which can remove some transitions and therefore make some raw nodes
   unreachable) and we assign consecutive numbers to the reachable nodes. *)

(* We define our own type [node] to be the number of a reachable node in this
   new numbering system. *)

type node =
  int

(* -------------------------------------------------------------------------- *)

(* All of the following mutable state is modified or initialized during the
   depth-first traversal below. *)

(* All of the arrays below are indexed by raw node numbers. *)

module M = DFS.MarkArray(Raw)

(* This array is initialized during the traversal. We assign new consecutive
   numbers to the reachable nodes. *)

let unreachable =
  -1

let _number : node array =
  Array.make Raw.n unreachable

let transport (raw_node : Raw.node) : node =
  _number.(Raw.encode raw_node)

(* This array of transitions is initialized here with the data supplied by
   [Raw.transitions]. Then, some transitions are *removed* (because of
   conflict resolution) during the traversal. *)

let transitions : Raw.node SymbolMap.t array =
  Array.init Raw.n (fun i ->
    Raw.transitions (Raw.decode i)
  )

(* Transitions are also stored in reverse, so as to allow reverse traversals
   of the automaton. This array is populated during the traversal. *)

let predecessors : node list array =
  Array.make Raw.n []

(* This array is initialized during the traversal. *)

let reductions : Reductions.t array =
  Array.make Raw.n TerminalMap.empty (* dummy *)

(* Tokens for which there are several possible actions in the raw LR(1)
   automaton are known as conflict tokens. This array is populated during the
   traversal. *)

let _conflict_tokens : TerminalSet.t array =
  Array.make Raw.n TerminalSet.empty

(* Information about end-of-stream conflicts in each state. *)

let _has_eos_conflict : (Production.t list * TerminalSet.t) option array =
  Array.make Raw.n None

(* (New as of 2012/01/23.) This flag records whether a shift/reduce conflict
   in this node was solved in favor of neither (%nonassoc). This is later used
   to forbid a default reduction at this node. *)

let forbid_default_reduction : bool array =
  Array.make Raw.n false

(* A count of all reachable nodes. *)

let n =
  ref 0

(* A list of nodes with conflicts. *)

let conflict_nodes : node list ref =
  ref []

(* Counts of nodes with shift/reduce and reduce/reduce conflicts. *)

let shift_reduce =
  ref 0

let reduce_reduce =
  ref 0

(* Count of the shift/reduce conflicts that could be silently
   resolved. *)

let silently_solved =
  ref 0

(* -------------------------------------------------------------------------- *)

(* A view of the raw LR(1) automaton as a graph. *)

(* This view relies on the [transitions] array, as opposed to the function
   [Raw.transitions]. This means that, once an edge has been removed, it can
   no longer be followed. *)

module RawForwardEdges = struct
  type node = Raw.node
  type label = Symbol.t
  let foreach_outgoing_edge node f =
    let i = Raw.encode node in
    SymbolMap.iter f transitions.(i)
  let foreach_root f =
    ProductionMap.iter (fun _prod node -> f node) Raw.entry
end

(* -------------------------------------------------------------------------- *)

module Precedence =
  Precedence.Make(Error)(Front)(Grammar)

(* -------------------------------------------------------------------------- *)

(* TODO CHANTIER

let () =
  Time.tick "Construction of the LR(1) automaton"
module A0 = Raw
module A1 = SilentConflictResolution.Run(A0)(Precedence)(Error)
let () = A1.diagnostics()
module A2 = Renumber.Run(A1)(Error)
let () = A2.diagnostics()
let () =
  if not Settings.ignore_all_unused_precedence_levels then
    Precedence.diagnostics()
module A3 = SevereConflictResolution.Run(A2)(Precedence)(Error)
let () = A3.diagnostics()
module Sites = ReductionSites.Run(A3)(Error)
module A4 = ExtraReductions.Run(A3)(Error)
let () = A4.diagnostics()

*)

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
  let key = Symbol.T t in
  transitions.(i) <- SymbolMap.remove key transitions.(i);
  true (* keep these reductions *)

and prefer_neither i t prods : bool =
  silently_solved := !silently_solved + List.length prods;
  let key = Symbol.T t in
  transitions.(i) <- SymbolMap.remove key transitions.(i);
  forbid_default_reduction.(i) <- true;
  false (* remove these reductions *)

and record_severe_conflict i t : bool =
  (* Record the existence of a severe shift/reduce conflict. *)
  _conflict_tokens.(i) <- TerminalSet.add t _conflict_tokens.(i);
  true (* keep these reductions *)

(* -------------------------------------------------------------------------- *)

(* This function is invoked during the traversal when a node is discovered. *)

(* It is in charge of detecting and resolving conflicts at this node. *)

let discover (raw_node : Raw.node) =
  let i = Raw.encode raw_node in

  (* Number this node. *)

  let node : node = MInt.postincrement n in
  _number.(i) <- node;

  (* Detect conflicts. We iterate over the reduction table. The code is
     written in such a way that we are aware of multi-way shift/reduce/reduce
     conflicts. We solve conflicts, when unambiguously allowed by priorities,
     by removing certain transitions and reductions. *)

  let hasSR = ref false
  and hasRR = ref false in

  let retained =
    Raw.reductions raw_node |> TerminalMap.filter @@ fun t prods ->
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
    _has_eos_conflict.(i) <- Some (prods, toks)
  end;

  (* Record statistics about conflicts. *)

  if not (TerminalSet.is_empty _conflict_tokens.(i)) then begin
    conflict_nodes := node :: !conflict_nodes;
    if !hasSR then
      incr shift_reduce;
    if !hasRR then
      incr reduce_reduce
  end

(* -------------------------------------------------------------------------- *)

(* This function is invoked during the traversal when an edge is traversed. *)

(* It records an edge in the predecessor array. *)

let traverse (source : Raw.node) _symbol (target : Raw.node) =
  (* The source node has been discovered and numbered already, so it can be
     transported. (This is not necessarily true of the target node.) *)
  let j = Raw.encode target in
  predecessors.(j) <- transport source :: predecessors.(j)

(* -------------------------------------------------------------------------- *)

(* Perform the depth-first traversal of the raw automaton. *)

let () =
  let module D = struct
    let traverse = traverse
    let discover = discover
  end in
  let module R = DFS.Run(RawForwardEdges)(M)(D) in
  ()

let () =
  if !silently_solved = 1 then
    Error.logA 1 (fun f ->
      Printf.fprintf f "One shift/reduce conflict was silently solved.\n"
    )
  else if !silently_solved > 1 then
    Error.logA 1 (fun f ->
      Printf.fprintf f "%d shift/reduce conflicts were silently solved.\n"
        !silently_solved
    );
  if !n < Raw.n then
    Error.logA 1 (fun f ->
      Printf.fprintf f
        "Only %d states remain after resolving shift/reduce conflicts.\n"
        !n
    )

let () =
  if not Settings.ignore_all_unused_precedence_levels then
    Precedence.diagnostics()

(* -------------------------------------------------------------------------- *)

(* Most of our mutable state becomes frozen at this point. Some transitions
   and reductions can still be removed further on, when default conflict
   resolution is performed. Also, some reductions can still be added further
   on, when [%on_error_reduce] declarations are obeyed. *)

let n =
  !n

let conflict_nodes =
  !conflict_nodes

(* We need a mapping of nodes to raw node numbers -- the inverse of the array
   [number]. *)

(* While building this mapping, we must remember that [number.(i)] is
   [unreachable] if the raw node number [i] has never been reached by the
   traversal. *)

let raw : node -> int =
  let raw = Array.make n (-1) (* dummy *) in
  Array.iteri (fun i (* raw index *) (node : node) ->
    assert (0 <= i && i < Raw.n);
    if node <> unreachable then begin
      assert (0 <= node && node < n);
      raw.(node) <- i
    end
  ) _number;
  fun node ->
    assert (0 <= node && node < n);
    raw.(node)

(* The array [transitions] is re-constructed so as to map nodes to nodes
   (instead of raw nodes to raw nodes). This array is now frozen; it is
   no longer modified. *)

let transitions : node SymbolMap.t array =
  Array.init n (fun node ->
    SymbolMap.map transport transitions.(raw node)
  )

(* The array [predecessors] is now frozen. *)

(* The array [reductions] is *not* yet frozen. *)

(* The array [conflict_tokens] is now frozen. *)

let () =
  Time.tick "Construction of the LR(1) automaton"

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

let encode node =
  node

let decode node =
  assert (0 <= node && node < n);
  node

let print node =
  Printf.sprintf "%d" (encode node)

let entry =
  ProductionMap.map transport Raw.entry

let state node =
  Raw.state (Raw.decode (raw node))

let transitions node =
  assert (0 <= node && node < n);
  transitions.(node)

let set_reductions node table =
  reductions.(raw node) <- table

let reductions node =
  reductions.(raw node)

let predecessors node =
  predecessors.(raw node)

let conflict_tokens node =
  _conflict_tokens.(raw node)

let conflicts f =
  List.iter (fun node ->
    f (conflict_tokens node) node
  ) conflict_nodes

let forbid_default_reduction node =
  forbid_default_reduction.(raw node)

let has_eos_conflict node =
  _has_eos_conflict.(raw node)

(* -------------------------------------------------------------------------- *)

(* The incoming symbol of a node can be computed by going through its LR(0)
   core. For this reason, we do not need to explicitly record it here. *)

let incoming_symbol node =
  Lr0.incoming_symbol (Lr0.ALR1.core (state node))

let is_start node =
  match incoming_symbol node with
  | None ->
      true
  | Some _ ->
      false

(* -------------------------------------------------------------------------- *)

(* Graph views. *)

module ForwardEdges = struct
  type nonrec node = node
  type label = Symbol.t
  let foreach_outgoing_edge node f =
    SymbolMap.iter f (transitions node)
end

module BackwardEdges = struct
  type nonrec node = node
  type label = unit (* could be changed to [Symbol.t option] if needed *)
  let foreach_outgoing_edge node f =
    List.iter (fun node -> f () node) (predecessors node)
end

(* -------------------------------------------------------------------------- *)

(* Iteration over all nodes. *)

let fold f accu =
  let accu = ref accu in
  for node = 0 to n - 1 do
    accu := f !accu node
  done;
  !accu

let iter f =
  for node = 0 to n - 1 do
    f node
  done

let map f =
  List.rev (
    fold (fun accu node ->
      f node :: accu
    ) []
  )

let foldx f =
  fold (fun accu node ->
    match incoming_symbol node with
    | None -> accu
    | Some _ -> f accu node
  )

let iterx f =
  iter (fun node ->
    match incoming_symbol node with
    | None -> ()
    | Some _ -> f node
  )

let tabulate (f : node -> 'a) =
  MArray.tabulate n f

let sum (f : node -> int) =
  MInt.sum n f

(* -------------------------------------------------------------------------- *)

(* We build a map of each symbol to the (reachable) nodes that have this
   incoming symbol. *)

let lookup symbol index =
  try SymbolMap.find symbol index with Not_found -> []

let index : node list SymbolMap.t =
  fold (fun index node ->
    match incoming_symbol node with
    | None ->
        index
    | Some symbol ->
        SymbolMap.add symbol (node :: lookup symbol index) index
  ) SymbolMap.empty

(* This allows iterating over all nodes that are targets of edges carrying a
   certain symbol. The sources of the corresponding edges are also provided. *)

let targets f accu symbol =
  (* There are no incoming transitions on the start symbols. *)
  let targets = lookup symbol index in
  List.fold_left (fun accu target ->
    f accu (predecessors target) target
  ) accu targets

let ntargets symbol =
  targets (fun count _ _ -> count + 1) 0 symbol

(* -------------------------------------------------------------------------- *)

(* Converting a start node into the single item that it contains. *)

let start2item node =
  let state : Lr0.ALR1.t = state node in
  let core : Lr0.node = Lr0.ALR1.core state in
  let items : Item.Set.t = Lr0.items core in
  assert (Item.Set.cardinal items = 1);
  Item.Set.choose items

(* -------------------------------------------------------------------------- *)
(* Computing which terminal symbols a state is willing to act upon.

   One must keep in mind that, due to the merging of states, a state might be
   willing to perform a reduction on a certain token, yet the reduction can
   take us to another state where this token causes an error. In other words,
   the set of terminal symbols that is computed here is really an
   over-approximation of the set of symbols that will not cause an error. And
   there seems to be no way of performing an exact computation, as we would
   need to know not only the current state, but the contents of the stack as
   well. *)

let acceptable_tokens (s : node) =

  (* If this state is willing to act on the error token, ignore it -- we do
     not wish to report that an error would be accepted in this state :-) *)

  let transitions =
    SymbolMap.remove (Symbol.T Terminal.error) (transitions s)
  and reductions =
    TerminalMap.remove Terminal.error (reductions s)
  in

  (* Accumulate the tokens carried by outgoing transitions. *)

  let covered =
    SymbolMap.fold (fun symbol _ covered ->
      match symbol with
      | Symbol.T tok ->
          TerminalSet.add tok covered
      | Symbol.N _ ->
          covered
    ) transitions TerminalSet.empty
  in

  (* Accumulate the tokens that permit reduction. *)

  let covered =
    ProductionMap.fold (fun _ toks covered ->
      TerminalSet.union toks covered
    ) (Reductions.reverse reductions) covered
  in

  (* That's it. *)

  covered

(* -------------------------------------------------------------------------- *)
(* Report statistics. *)

(* Produce the reports. *)

let () =
  if !shift_reduce = 1 then
    Error.warning [] "one state has shift/reduce conflicts."
  else if !shift_reduce > 1 then
    Error.warning [] "%d states have shift/reduce conflicts."
      !shift_reduce;
  if !reduce_reduce = 1 then
    Error.warning [] "one state has reduce/reduce conflicts."
  else if !reduce_reduce > 1 then
    Error.warning [] "%d states have reduce/reduce conflicts."
      !reduce_reduce

(* -------------------------------------------------------------------------- *)

(* Instantiate [Set] and [Map] on the type [node]. *)

module Node = struct
  type t = node
  let compare = (-)
end

module NodeSet = struct

  include Set.Make(Node)


  (* [union] does not guarantee physical equality between its second
     argument and its result when a logical equality holds. We wrap it
     so as to obtain this property. *)

  let leq_join s1 s2 =
    if subset s1 s2 then s2 else union s1 s2

  let print s =
    Printf.sprintf "{ %s }" (
      MString.separated_iter print ", " (fun f -> iter f s)
    )

end

module NodeMap =
  Map.Make(Node)

module ImperativeNodeMap =
  Fix.Glue.ArraysAsImperativeMaps(struct let n = n end)

let all_sources symbol =
  targets (fun accu sources _target ->
    List.fold_left (fun accu source -> NodeSet.add source accu) accu sources
  ) NodeSet.empty symbol

let all_targets symbol =
  targets (fun accu _sources target ->
    NodeSet.add target accu
  ) NodeSet.empty symbol

(* -------------------------------------------------------------------------- *)

(* For each production, compute where (that is, in which states) this
   production can be reduced. This computation is done AFTER default conflict
   resolution (see below). It is an error to call the accessor function
   [production_where] before default conflict resolution has taken place. *)

let production_where : NodeSet.t ProductionMap.t option ref =
  ref None

let initialize_production_where () =
  production_where := Some (
    fold (fun accu node ->
      TerminalMap.fold (fun _ prods accu ->
        let prod = MList.single prods in
        let nodes =
          try
            ProductionMap.find prod accu
          with Not_found ->
            NodeSet.empty
        in
        ProductionMap.add prod (NodeSet.add node nodes) accu
      ) (reductions node) accu
    ) ProductionMap.empty
  )

let production_where (prod : Production.t) : NodeSet.t =
  match !production_where with
  | None ->
      (* It is an error to call this function before conflict resolution. *)
      assert false
  | Some production_where ->
      try
        (* Production [prod] may be reduced at [nodes]. *)
        let nodes = ProductionMap.find prod production_where in
        assert (not (NodeSet.is_empty nodes));
        nodes
      with Not_found ->
        (* The production [prod] is never reduced. *)
        NodeSet.empty

(* -------------------------------------------------------------------------- *)
(* Warn about productions that are never reduced. *)

(* These are productions that can never, ever be reduced, because there is
   no state that is willing to reduce them. There could be other productions
   that are never reduced because the only states that are willing to reduce
   them are unreachable. We do not report those. In fact, through the use of
   the inspection API, it might be possible to bring the automaton into a
   state where one of those productions can be reduced. *)

let warn_about_productions_never_reduced () =
  let count = ref 0 in
  Production.iter (fun prod ->
    if NodeSet.is_empty (production_where prod) then
      match Production.test_start prod with
      | Some nt ->
          incr count;
          Error.warning
            (Nonterminal.positions nt)
            "symbol %s is never accepted." (Nonterminal.print false nt)
      | None ->
          incr count;
          Error.warning
            (Production.positions prod)
            "production %s is never reduced." (Production.print prod)
  );
  if !count > 0 then
    let plural_mark, be = if !count > 1 then ("s", "are") else ("", "is") in
    Error.warning []
      "in total, %d production%s %s never reduced." !count plural_mark be

(* -------------------------------------------------------------------------- *)
(* When requested by the code generator, apply default conflict
   resolution to ensure that the automaton is deterministic. *)

(* [best prod prods] chooses which production, out of the nonempty
   list [prod :: prods], should be reduced. If no best choice exists,
   one or more warnings are emitted and an arbitrary choice is made. *)

let rec best choice = function
  | [] ->
      choice
  | prod :: prods ->
      match Precedence.reduce_reduce choice prod with
      | Some choice ->
          best choice prods
      | None ->
          (* The cause for not knowing which production is best could be:
             1- the productions originate in different source files;
             2- they are derived, via inlining, from the same production. *)
          Error.warning
            (Production.positions choice @ Production.positions prod)
               "do not know how to resolve a reduce/reduce conflict\n\
                between the following two productions:\n%s\n%s"
                  (Production.print choice)
                  (Production.print prod);
          choice (* dummy *)

(* Go ahead. *)

let signal_conflicts kind n =
  if n = 1 then
    Error.warning [] "one %s conflict was arbitrarily resolved."
      kind
  else if n >= 1 then
    Error.warning [] "%d %s conflicts were arbitrarily resolved."
      n kind

let default_conflict_resolution () =

  let shift_reduce =
    ref 0
  and reduce_reduce =
    ref 0
  in

  conflict_nodes |> List.iter (fun node ->
    _conflict_tokens.(raw node) <- TerminalSet.empty;
    set_reductions node (
      TerminalMap.fold (fun tok prods reductions ->

        try
          let (_ : node) =
            SymbolMap.find (Symbol.T tok) (transitions node)
          in
          (* There is a transition at this symbol, so this
             is a (possibly multiway) shift/reduce conflict.
             Resolve in favor of shifting by suppressing all
             reductions. *)
          shift_reduce := List.length prods + !shift_reduce;
          reductions
        with Not_found ->
          (* There is no transition at this symbol. Check
             whether we have multiple reductions. *)
          match prods with
          | [] ->
              assert false
          | [ _ ] ->
              TerminalMap.add tok prods reductions
          | prod :: ((_ :: _) as prods) ->
              (* We have a reduce/reduce conflict. Resolve, if
                 possible, in favor of a single reduction.
                 This reduction must be preferable to each
                 of the others. *)
              reduce_reduce := List.length prods + !reduce_reduce;
              let best = best prod prods in
              TerminalMap.add tok [ best ] reductions

      ) (reductions node) TerminalMap.empty
    )
  );

  signal_conflicts "shift/reduce" !shift_reduce;
  signal_conflicts "reduce/reduce" !reduce_reduce;

  (* Now, detect and remove end-of-stream conflicts. If a state has both a
     reduce action at [#] and some other (shift or reduce) action, this is
     an end-of-stream conflict. This conflict is resolved by suppressing
     the reduce action at [#]. *)

  (* Because we have already removed some reductions above, we may find
     fewer end-of-stream conflicts than we did during our first pass. *)

  let eos_conflicts = ref 0 in

  iter begin fun node ->
    let transitions = transitions node
    and reductions = reductions node in

    if Reductions.has_eos_conflict transitions reductions then begin

      (* Suppress the reduce action at [#]. *)
      let _prods, reductions =
        TerminalMap.find_and_remove Terminal.sharp reductions in
      set_reductions node reductions;

      (* Mark this end-of-stream conflict as resolved. *)
      _has_eos_conflict.(raw node) <- None;

      (* Count this end-of-stream conflict. *)
      incr eos_conflicts

    end
  end;

  signal_conflicts "end-of-stream" !eos_conflicts;

  (* We can now compute where productions are reduced. *)
  initialize_production_where();
  warn_about_productions_never_reduced()

(* -------------------------------------------------------------------------- *)
(* Extra reductions. *)

(* 2015/10/19 Original implementation. *)
(* 2016/07/13 Use priority levels to choose which productions to reduce
              when several productions are eligible. *)

(* If a state can reduce some productions whose left-hand symbol has been
   marked [%on_error_reduce], and if one such production [prod] is preferable
   to every other (according to the priority rules of [%on_error_reduce]
   declarations), then every error action in this state is replaced with a
   reduction of [prod]. This is done even though this state may have outgoing
   shift transitions: thus, we are forcing one interpretation of the past,
   among several possible interpretations. *)

(* The code below looks like the decision on a default reduction in
   [Default], except we do not impose the absence of outgoing terminal
   transitions. Also, we actually modify the automaton, so the back-ends, the
   reference interpreter, etc., need not be aware of this feature, whereas
   they are aware of default reductions. *)

(* This code can run before we decide on the default reductions; this does
   not affect which default reductions will be permitted. *)

(* This code does not affect which productions can be reduced where. Thus,
   it is OK for it to run after [initialize_production_where()]. *)

(* A count of how many states receive extra reductions through this mechanism. *)

let extra =
  ref 0

(* A count of how many states have more than one eligible production, but one
   is preferable to every other (so priority plays a role). *)

let prioritized =
  ref []

(* The set of nonterminal symbols in the left-hand side of an extra reduction. *)

let extra_nts =
  ref NonterminalSet.empty

let extra_reductions_in_node node =
  (* Compute the productions which this node can reduce. *)
  let productions : _ ProductionMap.t = Reductions.reverse (reductions node) in
  let prods : Production.t list =
    ProductionMap.fold (fun prod _ prods -> prod :: prods) productions []
  in
  (* Keep only those whose left-hand symbol is marked [%on_error_reduce]. *)
  let prods = List.filter OnErrorReduce.reduce prods in
  (* Check if one of them is preferable to every other one. *)
  match MList.best OnErrorReduce.preferable prods with
  | None ->
      (* Either no production is marked [%on_error_reduce], or several of them
         are marked and none is preferable. *)
      ()
  | Some prod ->
      let acceptable = acceptable_tokens node in
      (* An extra reduction is possible. Replace every error action with
         a reduction of [prod]. If we replace at least one error action
         with a reduction, update [extra] and [extra_nts]. *)
      let triggered = lazy (
        incr extra;
        if List.length prods > 1 then
          prioritized := node :: !prioritized;
        extra_nts := NonterminalSet.add (Production.nt prod) !extra_nts
      ) in
      Terminal.iter_real (fun tok ->
        if not (TerminalSet.mem tok acceptable) then begin
          set_reductions node (TerminalMap.add tok [ prod ] (reductions node));
          Lazy.force triggered
        end
      )

let insert_extra_reductions () =
  (* Examine every node. *)
  iter (fun node ->
    (* Just like a default reduction, an extra reduction should be forbidden
       (it seems) if [forbid_default_reduction] is set. *)
    if not (forbid_default_reduction node) then
      extra_reductions_in_node node
  );
  (* Info message. *)
  if !extra > 0 then begin
    Error.logA 1 (fun f ->
      Printf.fprintf f "Extra reductions on error were added in %d states.\n"
        !extra;
      Printf.fprintf f "Priority played a role in %d of these states.\n"
        (List.length !prioritized)
    );
    Error.logA 2 (fun f ->
      if !prioritized <> [] then
        Printf.fprintf f "These states are %s.\n"
          (NodeSet.print (NodeSet.of_list !prioritized))
    )
  end;
  (* Warn about useless %on_error_reduce declarations. *)
  OnErrorReduce.iter (fun nt ->
    if not (NonterminalSet.mem nt !extra_nts) then
      Error.warning []
        "the declaration %%on_error_reduce %s is never useful."
        (Nonterminal.print false nt)
  )

(* -------------------------------------------------------------------------- *)
(* Define [fold_entry], which in some cases facilitates the use of [entry]. *)

let fold_entry f accu =
  ProductionMap.fold (fun prod state accu ->
    let nt = Option.get (Production.test_start prod) in
    let ty = Nonterminal.ocamltype_of_start_symbol nt in
    f prod state nt ty accu
  ) entry accu

let entry_of_nt nt =
  (* Find the entry state that corresponds to [nt]. *)
  try
    ProductionMap.find (Production.start_production nt) entry
  with Not_found ->
    assert false

exception Found of Nonterminal.t

let nt_of_entry s =
  (* [s] should be an initial state. *)
  assert (incoming_symbol s = None);
  try
    ProductionMap.iter (fun prod entry ->
      if Node.compare s entry = 0 then
        match Production.test_start prod with
        | None ->
            assert false
        | Some nt ->
            raise (Found nt)
    ) entry;
    (* This should not happen if [s] is indeed an initial state. *)
    assert false
  with Found nt ->
    nt
