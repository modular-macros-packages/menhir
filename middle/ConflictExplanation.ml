(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* TODO this algorithm must probably be applied to the LR(1) automaton after
   silent conflict resolution and before severe conflict resolutions, so the
   severe conflicts are still there to be seen (and the transitions removed
   by silent conflict reduction cannot be used to explain a conflict). *)

open MiddleAPI

module Run
(Lr1 : LR1)
(X : sig

  (**[construction_mode] determines which construction method was used.
     See [LR1Construction]. *)
  val construction_mode: [`Canonical | `InclusionOnly | `Pager | `LALR]

  (**[base] is the name of the [.conflicts] file that should be created,
     without the [.conflicts] extension. *)
  val base: string

end)
(E : sig

  (**[log format ...] emits an information message. *)
  val log: ('a, out_channel, unit, unit) format4 -> 'a

end)
= struct
module Lr0 = Lr1.Lr0
module G = Lr0.G
open G
open E
module Derivation = Derivation.Make(G)

(* -------------------------------------------------------------------------- *)

(* [materialize table x] turns an implicit list, stored using pointers through
   the hash table [table], into an explicit list. The head [x] of the implicit
   list is not included in the explicit list. *)

let materialize (table : ('a, 'a option) Hashtbl.t) (x : 'a) : 'a list =
  let rec loop x =
    match Hashtbl.find table x with
    | None ->
        []
    | Some x ->
        x :: loop x
  in
  loop x

(* -------------------------------------------------------------------------- *)
(* Explaining shift actions. *)

(* The existence of a shift action stems from the existence of a shift
   item in the LR(0) core that underlies the LR(1) state of interest.
   That is, lookahead sets are not relevant. The existence of a shift
   item in the LR(0) core is explained by finding a path from a start
   item to the shift item in the LR(0) nondeterministic automaton,
   such that the symbols read along this path form the (previously
   fixed) symbol string that leads to the conflict state in the LR(1)
   automaton. There may be several such paths: a shortest one is
   chosen. There may also be several shift items in the conflict
   state: an arbitrary one is chosen. I believe it would not be
   interesting to display traces for several shift items: they would
   be identical except in their last line (where the desired shift
   item actually appears). *)

(* Symbolic execution of the nondeterministic LR(0) automaton. *)

(* Configurations are pairs of an LR(0) item and an offset into
   the input string, which indicates how much has been read so
   far. *)

type configuration0 =
    Item.t * int

(* This function builds a derivation out of a (nonempty, reversed) sequence of
   configurations. The derivation is constructed from bottom to top, that is,
   beginning at the last configuration and moving back towards to the start
   configuration. *)

let rec follow derivation offset' = function
  | [] ->
      assert (offset' = 0);
      derivation
  | (item, offset) :: configs ->
      let prod, pos = Item.export item in
      let rhs = Production.rhs prod in
      let derivation =
        if offset = offset' then
          (* This is an epsilon transition. Put a new root node on top of
             the existing derivation. *)
          Derivation.build pos rhs derivation None
        else
          (* This was a shift transition. Tack symbol in front of the
             forest. *)
          Derivation.prepend rhs.(pos) derivation
      in
      follow derivation offset configs

(* Symbolic execution begins with a start item (corresponding
   to one of the automaton's entry nodes), a fixed string of
   input symbols, to be fully consumed, and a goal item. The
   objective is to find a path through the automaton that
   leads from the start configuration [(start, 0)] to the goal
   configuration [(stop, n)], where [n] is the length of the
   input string. The automaton is explored via breadth-first
   search. A hash table is used to record which configurations
   have been visited and to build a spanning tree of shortest
   paths. *)

exception Done

let explain_shift_item
    (start : Item.t)
    (input : Symbol.t array)
    (stop : Item.t)
    : Derivation.t =

  let n =
    Array.length input
  in

  let table : (configuration0, configuration0 option) Hashtbl.t =
    Hashtbl.create 1023
  in

  let queue : configuration0 Queue.t =
    Queue.create()
  in

  let enqueue ancestor config =
    try
      let _ = Hashtbl.find table config in
      ()
    with Not_found ->
      Hashtbl.add table config ancestor;
      Queue.add config queue
  in

  enqueue None (start, 0);
  try
    MQueue.repeat queue (function (item, offset) as config ->

      (* If the item we're looking at is the goal item and if
         we have read all of the input symbols, stop. *)

      if Item.equal item stop && offset = n then
        raise Done;

      (* Otherwise, explore the transitions out of this item. *)

      let prod, pos = Item.export item in
      let rhs = Production.rhs prod in
      let length = Array.length rhs in

      (* Shift transition, followed only if the symbol matches
         the symbol found in the input string. *)

      if pos < length
      && offset < n
      && Symbol.equal rhs.(pos) input.(offset) then begin
        let config' = (Item.import (prod, pos+1), offset+1) in
        enqueue (Some config) config'
      end;

      (* Epsilon transitions. *)

      if pos < length then
        match rhs.(pos) with
        | Symbol.N nt ->
            Production.iternt nt (fun prod ->
              let config' = (Item.import (prod, 0), offset) in
              enqueue (Some config) config'
            )
        | Symbol.T _ ->
            ()

    );
    assert false
  with Done ->

    (* We have found a (shortest) path from the start configuration to
       the goal configuration. Turn it into an explicit derivation. *)

    let configs = materialize table (stop, n) in
    let prod, pos = Item.export stop in
    let rhs = Production.rhs prod in
    let derivation = Derivation.tail pos rhs in
    let derivation = follow derivation n configs in
    derivation

(* -------------------------------------------------------------------------- *)
(* Explaining reduce actions. *)

(* The existence of a reduce action stems from the existence of a
   reduce item, whose lookahead set contains the token of interest, in
   the state of interest. Here, lookahead sets are relevant only
   insofar as they contain or do not contain the token of interest --
   in other words, lookahead sets can be abstracted by Boolean
   values. The existence of the reduce item is explained by finding a
   path from a start item to the reduce item in the LR(1)
   nondeterministic automaton, such that the symbols read along this
   path form the (previously fixed) symbol string that leads to the
   conflict state in the LR(1) automaton. There may be several such
   paths: a shortest one is chosen. *)

(* Symbolic execution of the nondeterministic LR(1) automaton. *)

(* Configurations are pairs of an LR(1) item and an offset into the
   input string, which indicates how much has been read so far. An
   LR(1) item is itself represented as the combination of an LR(0)
   item and a Boolean flag, telling whether the token of interest
   appears or does not appear in the lookahead set. *)

type configuration1 =
    Item.t * bool * int

(* This function builds a derivation out of a sequence of configurations. The
   end of the sequence is dealt with specially -- we want to explain how the
   lookahead symbol appears and is inherited. Once that is done, the rest
   (that is, the beginning) of the derivation is dealt with as above. *)

let config1toconfig0 (item, _, offset) =
  (item, offset)

let rec follow1 tok derivation offset' = function
  | [] ->
      assert (Terminal.equal tok Terminal.sharp);
      (* One could emit a comment saying that the lookahead token is
         initially [#]. That comment would have to be displayed above
         the derivation, though, and there is no support for that
         at the moment, so let's skip it. *)
      derivation
  | (item, _, offset) :: configs ->
      let prod, pos = Item.export item in
      let rhs = Production.rhs prod in
      let length = Array.length rhs in
      if offset = offset' then

        (* This is an epsilon transition. Attack a new line and add
           a comment that explains why the lookahead symbol is
           produced or inherited. *)

        let nullable, first = Analysis.nullable_first prod (pos + 1) in

        if TerminalSet.mem tok first then

          (* The lookahead symbol is produced (and perhaps also inherited,
             but let's ignore that). *)

          let e = Analysis.explain_first tok prod (pos + 1) in
          let comment =
            "lookahead token appears" ^ (if e = "" then "" else " because " ^ e)
          in
          let derivation =
            Derivation.build pos rhs derivation (Some comment)
          in

          (* Print the rest of the derivation without paying attention to
             the lookahead symbols. *)

          follow derivation offset (List.map config1toconfig0 configs)

        else begin

          (* The lookahead symbol is not produced, so it is definitely inherited. *)

          assert nullable;

          let comment =
            "lookahead token is inherited" ^
            (if pos + 1 < length then Printf.sprintf " because %s can vanish" (Symbol.print_subarray false (pos + 1) rhs) else "")
          in
          let derivation =
            Derivation.build pos rhs derivation (Some comment)
          in

          follow1 tok derivation offset configs

        end

      else

        (* This is a shift transition. Tack symbol in front of forest. *)

        let derivation =
          Derivation.prepend rhs.(pos) derivation
        in

        follow1 tok derivation offset configs

(* Symbolic execution is performed in the same manner as above. *)

let explain_reduce_item
    (tok : Terminal.t)
    (start : Item.t)
    (input : Symbol.t array)
    (stop : Item.t)
    : Derivation.t =

  let n =
    Array.length input
  in

  let table : (configuration1, configuration1 option) Hashtbl.t =
    Hashtbl.create 1023
  in

  let queue : configuration1 Queue.t =
    Queue.create()
  in

  let enqueue ancestor config =
    try
      let _ = Hashtbl.find table config in
      ()
    with Not_found ->
      Hashtbl.add table config ancestor;
      Queue.add config queue
  in

  (* If the lookahead token is #, then it initially appear in the lookahead
     set, otherwise it doesn't. *)

  enqueue None (start, Terminal.equal tok Terminal.sharp, 0);
  try
    MQueue.repeat queue (function (item, lookahead, offset) as config ->

      (* If the item we're looking at is the goal item and if
         we have read all of the input symbols, stop. *)

      if Item.equal item stop && lookahead && offset = n then
        raise Done;

      (* Otherwise, explore the transitions out of this item. *)

      let prod, pos = Item.export item in
      let rhs = Production.rhs prod in
      let length = Array.length rhs in

      (* Shift transition, followed only if the symbol matches
         the symbol found in the input string. *)

      if pos < length
      && offset < n
      && Symbol.equal rhs.(pos) input.(offset) then begin
        let config' = (Item.import (prod, pos+1), lookahead, offset+1) in
        enqueue (Some config) config'
      end;

      (* Epsilon transitions. *)

      if pos < length then
        match rhs.(pos) with
        | Symbol.N nt ->
            let nullable, first = Analysis.nullable_first prod (pos + 1) in
            let first : bool = TerminalSet.mem tok first in
            let lookahead' =
              if nullable then first || lookahead else first
            in
            Production.iternt nt (fun prod ->
              let config' = (Item.import (prod, 0), lookahead', offset) in
              enqueue (Some config) config'
            )
        | Symbol.T _ ->
            ()

    );
    assert false
  with Done ->

    (* We have found a (shortest) path from the start configuration to
       the goal configuration. Turn it into an explicit derivation. *)

    let configs = materialize table (stop, true, n) in
    let derivation = Derivation.empty in
    let derivation = follow1 tok derivation n configs in
    derivation

(* -------------------------------------------------------------------------- *)
(* A counter of how many conflicts could *not* be explained. *)

(* When this counter is nonzero, we display a message on the standard output
   channel. This can help us detect regressions via [make test]. *)

let unexplainable =
  ref 0

let log_unexplainable () =
  if !unexplainable > 0 then
    log
      "%d conflict%s could not be explained.\n"
      !unexplainable
      (if !unexplainable > 1 then "s" else "")

(* -------------------------------------------------------------------------- *)

(* Enumerating all nodes with a conflict. *)

let[@inline] has_shift_reduce_conflict node t =
  SymbolMap.mem (Symbol.T t) (Lr1.transitions node) &&
  TerminalMap.mem t (Lr1.reductions node)

let[@inline] has_reduce_reduce_conflict node t =
  match TerminalMap.find t (Lr1.reductions node) with
  | prods ->
      List.length prods > 1
  | exception Not_found ->
      false

let foreach_conflict_node yield =
  Lr1.iter @@ fun node ->
  let ts =
    Terminal.fold (fun t ts ->
      if has_shift_reduce_conflict node t
      || has_reduce_reduce_conflict node t
      then TerminalSet.add t ts
      else ts
    ) TerminalSet.empty
  in
  if not (TerminalSet.is_empty ts) then
    yield node ts

(* -------------------------------------------------------------------------- *)
(* Putting it all together. *)

let () =

  (* 2018/09/05: when [--explain] is enabled, always create a fresh
     .conflicts file (wiping out any pre-existing file), even if
     there are in fact no conflicts. This should avoid confusion with
     outdated .conflicts files. *)

  let out =
    open_out (X.base ^ ".conflicts")
  in

  foreach_conflict_node (fun node toks ->
  try

    (* Construct a partial LR(1) automaton, looking for a conflict
       in a state that corresponds to this node. Because Pager's
       algorithm can merge two states as soon as one of them has a
       conflict, we can't be too specific about the conflict that we
       expect to find in the canonical automaton. So, we must supply
       a set of conflict tokens and accept any kind of conflict that
       involves one of them. *)

    (* TEMPORARY with the new compatibility criterion, we can be
       sure that every conflict token is indeed involved in a
       conflict. Exploit that? Avoid focusing on a single token? *)

    let module P = LR1Partial.Run(Lr1)(struct
      let goal = node
      let conflicts = toks
    end) in

    assert (Item.is_start P.source);

    let closure =
      Lr0.CLR1.closure P.goal in

    (* Determine what kind of conflict was found. *)

    let shift, reduce = Item.Map.fold (fun item toks (shift, reduce) ->
      match Item.classify item with
      | Item.Shift (Symbol.T tok, _)
        when Terminal.equal tok P.t ->
            shift + 1, reduce
      | Item.Reduce _
        when TerminalSet.mem P.t toks ->
          shift, reduce + 1
      | _ ->
          shift, reduce
    ) closure (0, 0) in

    let kind =
      if shift > 0 && reduce > 1 then
        "shift/reduce/reduce"
      else if (shift > 0) then
        "shift/reduce"
      else
        "reduce/reduce"
    in

    (* Explain how the conflict state is reached. *)

    Printf.fprintf out "\n\
      ** Conflict (%s) in state %d.\n\
      ** Token%s involved: %s\n%s\
      ** This state is reached from %s after reading:\n\n%s\n"
    kind (Lr1.encode node)
    (if TerminalSet.cardinal toks > 1 then "s" else "")
    (TerminalSet.print toks)
    (if TerminalSet.cardinal toks > 1 then
      Printf.sprintf "** The following explanations concentrate on token %s.\n"
        (Terminal.print P.t)
    else "")
    (Nonterminal.print false (Item.get_start P.source))
    (Symbol.print_array false P.path);

    (* Examine the items in that state, focusing on one particular
       token. Out of the shift items, we explain just one -- this
       seems enough. We explain each of the reduce items. *)

    (* First, build a mapping of items to derivations. *)

    let (_ : bool), derivations =
      Item.Map.fold (fun item toks (still_looking_for_shift_item, derivations) ->
        match Item.classify item with

        | Item.Shift (Symbol.T tok, _)
          when still_looking_for_shift_item && Terminal.equal tok P.t ->

            false,
            let derivation = explain_shift_item P.source P.path item in
            Item.Map.add item derivation derivations

        | Item.Reduce _
          when TerminalSet.mem P.t toks ->

            still_looking_for_shift_item,
            let derivation = explain_reduce_item P.t P.source P.path item in
            Item.Map.add item derivation derivations

        | _ ->

            still_looking_for_shift_item,
            derivations

      ) closure (true, Item.Map.empty)
    in

    (* Factor out the common context among all derivations, so as to avoid
       repeating it. This helps prevent derivation trees from drifting too
       far away towards the right. It also helps produce sub-derivations
       that are quite compact. *)

    let context, derivations =
      Derivation.factor derivations
    in

    (* Display the common context. *)

    Printf.fprintf out
      "\n** The derivations that appear below have the following common factor:\
       \n** (The question mark symbol (?) represents the spot where the derivations begin to differ.)\n\n";
    Derivation.printc out context;

    (* Then, display the sub-derivations. *)

    Item.Map.iter (fun item derivation ->

      Printf.fprintf out
        "\n** In state %d, looking ahead at %s, "
        (Lr1.encode node)
        (Terminal.print P.t);

      begin match Item.classify item with
      | Item.Shift _ ->
          Printf.fprintf out "shifting is permitted\n** because of the following sub-derivation:\n\n"
      | Item.Reduce prod ->
          Printf.fprintf out
            "reducing production\n** %s\n** is permitted because of the following sub-derivation:\n\n"
            (Production.print prod)
      end;

      Derivation.print out derivation

    ) derivations;

    flush out

  with LR1Partial.Oops ->

    (* Ha ha! We were unable to explain this conflict. This could happen
       because the automaton was butchered by conflict resolution directives,
       or because [--lalr] was enabled and we have unexplainable LALR conflicts.
       Anyway, send the error message to the .conflicts file and continue. *)

    incr unexplainable;
    Printf.fprintf out "\n\
      ** Conflict (unexplainable) in state %d.\n\
      ** Token%s involved: %s\n\
      ** %s.\n%!"
    (Lr1.encode node)
    (if TerminalSet.cardinal toks > 1 then "s" else "")
    (TerminalSet.print toks)
    (match X.construction_mode with
    | `LALR ->
        "This may be an artificial conflict caused by your use of --lalr"
    | `Canonical
    | `InclusionOnly
    | `Pager ->
        "Please send your grammar to Menhir's developers"
    )

  );
  close_out out;
  log_unexplainable()

end (* Run *)
