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

(* Make sure that the middle-end runs before the automaton is constructed. *)
include Middle

(* -------------------------------------------------------------------------- *)

(* [Closure] performs a precomputation which later helps efficiently compute
   the closure of an LR(0) or LR(1) state. This precomputation requires linear
   time in the size of the grammar. In this precomputation, the nature of the
   lookahead sets is abstract. The parameter [L] represents the lookahead
   sets. *)

module Closure (L : sig

  (**The type of lookahead sets. *)
  type t

  (**The empty lookahead set. *)
  val empty: t

  (**A concrete, constant set of terminal symbols. *)
  val constant: Grammar.TerminalSet.t -> t

  (**The union of two lookahead sets. *)
  val union: t -> t -> t

end) : sig

  (**A state is a map of items to lookahead information. *)
  type state = L.t Item.Map.t

  (**[closure] computes the closure of a state through all epsilon
     transitions. *)
  val closure: state -> state

end = struct

  (* The precomputation builds the LR(0) nondeterministic automaton. This is
     a graph whose nodes are items and whose edges are epsilon transitions.
     (We do not care about shift transitions here.) Lookahead information
     can be attached to nodes and is propagated through the graph during
     closure computations. *)

  (* All of the epsilon transitions that leave a node have the same behavior
     with respect to lookahead information. *)

  (* The lookahead set that is transmitted along an epsilon transition is
     either a constant, or the union of a constant and the lookahead set at
     the source node. The former case corresponds to a source item whose
     trailer is not nullable; the latter case corresponds to a source item
     whose trailer is nullable. *)

  type state =
    L.t Item.Map.t

  type node = {

      item: Item.t;
        (**Each node represents an item. *)

      constant: L.t;
        (**A constant lookahead set that is transmitted along every outgoing
           epsilon transition. *)

      transmits: bool;
        (**This Boolean flag indicates whether the lookahead set at this
           node is or is not transmitted along every outgoing transition. *)

      mutable transitions: node list;
        (**This node's successors through epsilon transitions.
           This field is never modified once initialization is over. *)

      (* The following fields are transient: that is, they are used only
         temporarily, during graph traversals. *)

      mutable mark: Mark.t;
        (**A (transient) mutable mark, used during graph traversal. *)

      mutable predecessors: node list;
         (**A (transient) list of predecessors, used to record which edges
            have been traversed. *)

      mutable lookahead: L.t;
        (**A (transient) lookahead set at this node. *)

    }

  (* Allocate one graph node per item. Build a mapping of items to nodes. *)

  let init prod pos : node =
    let item = Item.import (prod, pos) in
    (* The lookahead set that is transmitted through an epsilon transition
       is the FIRST set of the remainder of the source item, plus, if that
       is nullable, the lookahead set of the source item. *)
    let constant, transmits =
      if pos < Production.length prod then
        let nullable, first = Analysis.nullable_first prod (pos + 1) in
        L.constant first, nullable
      else
        (* No epsilon transitions leave this item. *)
        L.empty, false
    and transitions = [] (* temporary placeholder *)
    and mark = Mark.none
    and predecessors = []
    and lookahead = L.empty
    in
    { item; constant; transmits; transitions; mark; predecessors; lookahead }

  let mapping : Production.t -> node array =
    Production.tabulate @@ fun prod ->
    Array.init (Production.length prod + 1) @@ fun pos ->
    init prod pos

  let item2node item : node =
    let prod, pos = Item.export item in
    (mapping prod).(pos)

  (* At each node, compute outgoing transitions. *)

  let () =
    Production.iter @@ fun prod ->
    let rhs = Production.rhs prod in
    mapping prod |> Array.iteri @@ fun pos node ->
    node.transitions <-
      if pos < Array.length rhs then
        match rhs.(pos) with
        | Symbol.N nt ->
            Production.mapnt nt @@ fun prod ->
            item2node (Item.import (prod, 0))
        | Symbol.T _ ->
            []
      else
        []

  (* We can be certain that there are no cycles of transitions that transmit
     a lookahead set. This guarantees that we can traverse these transitions
     in a topological order.

     Indeed, if there was such a cycle, then every item in this cycle would
     have to be of the form A -> . B beta, where beta is nullable. DeRemer
     and Pennello (1982) call this an includes cycle. An includes cycle is a
     special case of a cycle, as defined by Aho and Ullman. The module
     [LoopDetection] detects and rejects cycles, so we can be assured at
     this point that no such cycle exists. *)

  (* Closure computation. *)

  let closure (items : state) : state =

    (* Explore the graph forwards, starting from these items. Marks are used
       to tell which nodes have been visited. Build a list of all visited
       nodes; this is in fact the list of all items in the closure.

       At initial nodes and when reaching a node through a transition,
       record a lookahead set.

       When we reach a node through a transition that transmits the
       lookahead set found at its source, record its source, so as to
       allow re-traversing this transition backwards (below). *)

    let this = Mark.fresh() in
    let nodes = ref [] in

    let rec visit father transmits toks node =
      if Mark.same node.mark this then begin
        (* Node has been visited already. *)
        node.lookahead <- L.union toks node.lookahead;
        if transmits then
          node.predecessors <- father :: node.predecessors
      end
      else begin
        (* Node is new. *)
        node.predecessors <- if transmits then [ father ] else [];
        node.lookahead <- toks;
        follow node
      end

    and follow node =
      node.mark <- this;
      nodes := node :: !nodes;
      List.iter
        (visit node node.transmits node.constant)
        node.transitions

    in

    Item.Map.iter (fun item toks ->
      let node = item2node item in
      visit node (* dummy! *) false toks node
    ) items;

    let nodes =
      !nodes in

    (* Explore the graph of transmitting transitions backwards. By
       hypothesis, it is acyclic, so this is a topological walk.
       Lookahead sets are inherited through transitions. *)

    let this = Mark.fresh() in

    let rec walk node =
      if not (Mark.same node.mark this) then begin
        (* Node is new. *)
        node.mark <- this;
        (* Explore all predecessors and merge their lookahead
           sets into the current node's own lookahead set. *)
        node.predecessors |> List.iter @@ fun predecessor ->
        walk predecessor;
        node.lookahead <- L.union predecessor.lookahead node.lookahead
      end
    in

    List.iter walk nodes;

    (* Done. Produce a mapping of items to lookahead sets.
       Clear all transient fields so as to reduce pressure
       on the GC -- this does not make much difference. *)

    List.fold_left (fun closure node ->
      node.predecessors <- [];
      let closure = Item.Map.add node.item node.lookahead closure in
      node.lookahead <- L.empty;
      closure
    ) Item.Map.empty nodes

end (* Closure *)

(* -------------------------------------------------------------------------- *)

(* A set variable is represented as an integer value. *)

(* The number of distinct set variables that we need is not easily known
   ahead of time; it is the maximum number of items in a state. *)

module VarSet =
  Bitsets.SparseBitSet

(* -------------------------------------------------------------------------- *)

(* Symbolic lookahead sets. *)

module SymbolicLookahead = struct

  (* A symbolic lookahead set consists of a concrete set of terminal symbols
     and of a number of set variables. *)

  type t =
    TerminalSet.t * VarSet.t

  let constant ts =
    (ts, VarSet.empty)

  let empty =
    constant TerminalSet.empty

  let union (ts1, vars1) ((ts2, vars2) as s2) =
    let ts = TerminalSet.union ts1 ts2
    and vars = VarSet.union vars1 vars2 in
    if ts2 == ts && vars2 == vars then s2 else
    (ts, vars)

  let variable (var : int) : t =
    (TerminalSet.empty, VarSet.singleton var)

  let project (ts, vars) =
    assert (VarSet.is_empty vars);
    ts

end

(* We perform closure computations with symbolic lookahead sets.
   This allows us to later represent LR(1) states as pairs of an
   LR(0) node number and an array of concrete lookahead sets. *)

module SymbolicClosure =
  Closure(SymbolicLookahead)

(* Closure operations over concrete lookahead sets are also needed when
   explaining conflicts. One could take another instance of the functor
   [Closure]. The approach below is somewhat less elegant and makes each call
   to [closure] somewhat slower, but saves the cost of instantiating the
   functor again. This cost is linear in the size of the grammar. *)

type concretelr1state =
  TerminalSet.t Item.Map.t

let closure (state : concretelr1state) : concretelr1state =
  state
  |> Item.Map.map SymbolicLookahead.constant
  |> SymbolicClosure.closure
  |> Item.Map.map SymbolicLookahead.project

(* -------------------------------------------------------------------------- *)

(* [transitions state] computes the non-epsilon transitions that leave the
   state [state]. Here, a state is a set of items. This code is parametric
   in the type of the lookahead sets. *)

let transitions (state : 'a Item.Map.t) : 'a Item.Map.t SymbolMap.t =
  Item.Map.fold (fun item ts transitions ->
    match Item.classify item with
    | Item.Shift (symbol, item') ->
        let items : 'a Item.Map.t =
          try
            SymbolMap.find symbol transitions
          with Not_found ->
            Item.Map.empty
        in
        SymbolMap.add symbol (Item.Map.add item' ts items) transitions
    | Item.Reduce _ ->
        transitions
  ) state SymbolMap.empty

(* -------------------------------------------------------------------------- *)

(* [reductions state] computes the reduction opportunities at the (closed)
   state [state]. These opportunities are represented as a list of pairs of a
   lookahead set and a production index. This code is again parametric in the
   type of the lookahead sets. *)

let reductions (state : 'a Item.Map.t) : ('a * Production.t) list =
  Item.Map.fold (fun item ts accu ->
    match Item.classify item with
    | Item.Reduce prod ->
        (ts, prod) :: accu
    | Item.Shift _ ->
        accu
  ) state []

(* -------------------------------------------------------------------------- *)

(* Construction of the the LR(0) automaton. *)

(* Nodes are numbered sequentially. *)

type node =
    int

(* A symbolic transition is a pair of the target state number and an array of
   symbolic lookahead sets. The variables in these sets are numbered in [0,g)
   where g is the number of items in the source LR(0) state. Items are
   numbered in the order of presentation by [Item.Set.fold]. *)

type symbolic_transition_target =
    node * SymbolicLookahead.t array

(* The automaton is represented by (growing) arrays of states (sets of items),
   symbolic transition information, and symbolic reduction information,
   indexed by node numbers. Conversely, a hash table maps states (sets of
   items) to node numbers. *)

let n =
  ref 0

let states : Item.Set.t InfiniteArray.t =
  InfiniteArray.make Item.Set.empty

let _transitions : symbolic_transition_target SymbolMap.t InfiniteArray.t =
  InfiniteArray.make SymbolMap.empty

let _reductions : (SymbolicLookahead.t * Production.t) list InfiniteArray.t =
  InfiniteArray.make []

let map : (Item.Set.t, node) Hashtbl.t =
  Hashtbl.create 50021

let incoming : Symbol.t option InfiniteArray.t =
  InfiniteArray.make None

(* The automaton is built depth-first. *)

let rec explore (symbol : Symbol.t option) (state : Item.Set.t) : node =

  (* Find out whether this state was already explored. *)

  try
    Hashtbl.find map state
  with Not_found ->

    (* If not, create a new node. *)

    let k = !n in
    n := k + 1;
    InfiniteArray.set states k state;
    Hashtbl.add map state k;

    (* Record its incoming symbol. *)

    InfiniteArray.set incoming k symbol;

    (* Build a symbolic version of the current state, where each item
       is associated with a distinct lookahead set variable, numbered
       consecutively. *)

    let (_ : int), (symbolic_state : SymbolicClosure.state) =
      Item.Set.fold (fun item (i, symbolic_state) ->
        i+1, Item.Map.add item (SymbolicLookahead.variable i) symbolic_state
      ) state (0, Item.Map.empty) in

    (* Compute the symbolic closure. *)

    let closure = SymbolicClosure.closure symbolic_state in

    (* Compute symbolic information about reductions. *)

    InfiniteArray.set _reductions k (reductions closure);

    (* Compute symbolic information about the transitions, and, by dropping
       the symbolic lookahead information, explore the transitions to further
       LR(0) states. *)

    InfiniteArray.set _transitions k (SymbolMap.mapi (fun symbol symbolic_state ->
      let (k : node) = explore (Some symbol) (Item.Map.domain symbolic_state) in
      let lookahead : SymbolicLookahead.t array =
        Array.make (Item.Map.cardinal symbolic_state) SymbolicLookahead.empty in
      let (_ : int) = Item.Map.fold (fun _ s i ->
        lookahead.(i) <- s;
        i+1
      ) symbolic_state 0 in
      ((k, lookahead) : symbolic_transition_target)
    ) (transitions closure));

    k

(* [start prod] is the start state that corresponds to the start production
   [prod]. It contains a single item, consisting of the start production, at
   position 0. *)

let start prod : Item.Set.t =
   Item.Set.singleton (Item.import (prod, 0))

(* The construction of the LR(0) automaton is started here. The entry nodes
   are recorded in the array [entry]. *)

let entry : node ProductionMap.t =
  ProductionMap.start (fun prod ->
    explore None (start prod)
  )

let () =
  Hashtbl.clear map

let n =
  !n

let () =
  Error.logA 1 (fun f -> Printf.fprintf f "Built an LR(0) automaton with %d states.\n" n);
  Time.tick "Construction of the LR(0) automaton"

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

let items node : Item.Set.t =
  InfiniteArray.get states node

let incoming_symbol node : Symbol.t option =
  InfiniteArray.get incoming node

let outgoing_edges node : node SymbolMap.t =
  SymbolMap.map
    (fun (target, _) -> target)
    (InfiniteArray.get _transitions node)

let outgoing_symbols node : Symbol.t list =
  SymbolMap.domain (InfiniteArray.get _transitions node)

(* -------------------------------------------------------------------------- *)

(* Help for building the LR(1) automaton. *)

(* An LR(1) state is represented as a pair of an LR(0) state number
   and an array of concrete lookahead sets (whose length depends on
   the LR(0) state). *)

type lr1state =
    node * TerminalSet.t array

(* A view of the type [lr1state] as an ordered type. This can be used in
   conjuction with [Fix.Memoize], [Fix.Numbering], etc. E.g. a numbering
   facility based on this mechanism is able to number 10000 states in
   about 0.01s. *)

module Lr1StateAsOrderedType = struct
  type t = lr1state
  let compare (k1, tsr1) (k2, tsr2) =
    let c = k1 - k2 in
    if c <> 0 then c
    else MArray.compare TerminalSet.compare tsr1 tsr2
end

(* An encoded LR(1) state can be turned into a concrete representation,
   that is, a mapping of items to concrete lookahead sets. *)

let export (k, tsr) =
  let (_ : int), items = Item.Set.fold (fun item (i, items) ->
    i+1, Item.Map.add item tsr.(i) items
  ) (InfiniteArray.get states k) (0, Item.Map.empty) in
  items

(* Displaying a concrete state. *)

let print_concrete leading (state : concretelr1state) =
  MString.with_buffer 1024 (fun buffer ->
    Item.Map.iter (fun item ts ->
      Printf.bprintf buffer "%s%s [ %s ]\n"
        leading
        (Item.print item)
        (TerminalSet.print ts)
    ) state
  )

(* Displaying a state. By default, only the kernel is displayed, not
   the closure. *)

let print leading state =
  print_concrete leading (export state)

(* The core of an LR(1) state is the underlying LR(0) state. *)

let core (k, _) =
  k

(* A sanity check. This well-formedness check is quite costly, due to the
   use of [Item.Set.cardinal]. Therefore, it is enabled only when [debug]
   is [true]. *)

let debug =
  false

let well_formed (k, tsr) =
  not debug ||
  Array.length tsr = Item.Set.cardinal (InfiniteArray.get states k)

(* An LR(1) start state is the combination of an LR(0) start state
   (which consists of a single item) with a singleton lookahead set
   that consists of the special terminal symbol [#]. *)

let start k =
  let state = (k, [| TerminalSet.singleton Terminal.sharp |]) in
  assert (well_formed state);
  state

(* Interpreting a symbolic lookahead set with respect to a source
   state. The variables in the symbolic lookahead set (which are
   integers) are interpreted as indices into the state's array of
   concrete lookahead sets. The result is a concrete lookahead set. *)

let interpret
    ((_, tsr) as state : lr1state)
    ((ts, vars) : SymbolicLookahead.t)
    : TerminalSet.t =

  assert (well_formed state);
  VarSet.fold (fun var ts ->
    assert (var >= 0 && var < Array.length tsr);
    TerminalSet.union tsr.(var) ts
  ) vars ts

(* Out of an LR(1) state, one produces information about reductions
   and transitions. This is done in an efficient way by interpreting
   the precomputed symbolic information with respect to that state. *)

let reductions
    ((k, _) as state : lr1state)
    : (TerminalSet.t * Production.t) list =

  List.map (fun (s, prod) ->
    interpret state s, prod
  ) (InfiniteArray.get _reductions k)

let transitions
    ((k, _) as state : lr1state)
    : lr1state SymbolMap.t =

  SymbolMap.map (fun ((k, sr) : symbolic_transition_target) ->
    ((k, Array.map (interpret state) sr) : lr1state)
  ) (InfiniteArray.get _transitions k)

let transition
    symbol
    ((k, _) as state : lr1state)
    : lr1state =

  let ((k, sr) : symbolic_transition_target) =
    try
      SymbolMap.find symbol (InfiniteArray.get _transitions k)
    with Not_found ->
      assert false (* no transition along this symbol *)
  in
  (k, Array.map (interpret state) sr)

(* [transition_tokens transitions] returns the set of tokens (terminal symbols)
   that are labels of outgoing transitions in the table [transitions]. *)

let transition_tokens transitions =
  SymbolMap.fold (fun symbol _target ts ->
    match symbol with
    | Symbol.T tok -> TerminalSet.add tok ts
    | Symbol.N _   -> ts
  ) transitions TerminalSet.empty

(* Equality of states. *)

let equal ((k1, tsr1) as state1) ((k2, tsr2) as state2) =
  assert (k1 = k2 && well_formed state1 && well_formed state2);
  let rec loop i =
    if i = 0 then
      true
    else
      let i = i - 1 in
      (TerminalSet.equal tsr1.(i) tsr2.(i)) && (loop i)
  in
  loop (Array.length tsr1)

(* Subsumption between states. *)

let subsume ((k1, tsr1) as state1) ((k2, tsr2) as state2) =
  assert (k1 = k2 && well_formed state1 && well_formed state2);
  let rec loop i =
    if i = 0 then
      true
    else
      let i = i - 1 in
      (TerminalSet.subset tsr1.(i) tsr2.(i)) && (loop i)
  in
  loop (Array.length tsr1)

(* This function determines whether two (core-equivalent) states are
   compatible, according to a criterion that is close to Pager's weak
   compatibility criterion.

   Pager's criterion guarantees that if a merged state has a potential
   conflict at [(i, j)] -- that is, some token [t] appears within the
   lookahead sets of both item [i] and item [j] -- then there exists a
   state in the canonical automaton that also has a potential conflict
   at [(i, j)] -- that is, some token [u] appears within the lookahead
   sets of both item [i] and item [j]. Note that [t] and [u] can be
   distinct.

   Pager has shown that his weak compatibility criterion is stable,
   that is, preserved by transitions and closure. This means that, if
   two states can be merged, then so can their successors. This is
   important, because merging two states means committing to merging
   their successors, even though we have not even built these
   successors yet.

   The criterion used here is a slightly more restrictive version of
   Pager's criterion, which guarantees equality of the tokens [t] and
   [u]. This is done essentially by applying Pager's original
   criterion on a token-wise basis. Pager's original criterion states
   that two states can be merged if the new state has no conflict or
   one of the original states has a conflict. Our more restrictive
   criterion states that two states can be merged if, for every token
   [t], the new state has no conflict at [t] or one of the original
   states has a conflict at [t].

   This modified criterion is also stable. My experiments show that it
   is almost as effective in practice: out of more than a hundred
   real-world sample grammars, only one automaton was affected, and
   only one extra state appeared as a result of using the modified
   criterion. Its advantage is to potentially make conflict
   explanations easier: if there appears to be a conflict at [t], then
   some conflict at [t] can be explained. This was not true when using
   Pager's original criterion. *)

(* A word of caution: reasoning about compatibility is tricky and often
   counter-intuitive. Here is a list of properties and non-properties:

   - Compatibility is reflexive and symmetric.

   - Compatibility is *not* transitive.

   - If two states A and B are in the subumption relation (i.e., one is a
     subset of the other), then A and B are compatible.

   - Compatibility is *not* monotonic. That is, it is *not* the case that if
     two states A and B are incompatible, then two larger states A' and B'
     must be incompatible as well. (The fact that the state A U B is
     compatible with itself shows that this is false.) In the contrapositive,
     it is *not* the case that if A and B are compatible, then two smaller
     states A' and B' must be compatible as well.

   - Compatibility is preserved by union of compatible states. That is, if
     A and B are compatible, then C is compatible with (A U B) if and only
     if C is compatible with both A and B. *)

let compatible (k1, tsr1) (k2, tsr2) =
  assert (k1 = k2);
  let n = Array.length tsr1 in
  (* Two states are compatible if and only if they are compatible
     at every pair (i, j), where i and j are distinct. *)
  let rec loopi i =
    if i = n then
      true
    else
      let tsr1i = tsr1.(i)
      and tsr2i = tsr2.(i) in
      let rec loopj j =
        if j = i then
          true
        else
          let tsr1j = tsr1.(j)
          and tsr2j = tsr2.(j) in

          (* The two states are compatible at (i, j) if every conflict
             token in the merged state already was a conflict token in
             one of the two original states. This could be written as
             follows:

            TerminalSet.subset
              (TerminalSet.inter (TerminalSet.union tsr1i tsr2i) (TerminalSet.union tsr1j tsr2j))
              (TerminalSet.union (TerminalSet.inter tsr1i tsr1j) (TerminalSet.inter tsr2i tsr2j))

             but is easily seen (on paper) to be equivalent to:

          *)

             TerminalSet.subset
               (TerminalSet.inter tsr2i tsr1j)
               (TerminalSet.union tsr1i tsr2j)
          &&
             TerminalSet.subset
               (TerminalSet.inter tsr1i tsr2j)
               (TerminalSet.union tsr2i tsr1j)
          &&
             loopj (j+1)
      in
      loopj 0 && loopi (i+1)
  in
  loopi 0

(* This function determines whether two (core-equivalent) states can
   be merged without creating an end-of-stream conflict, now or in the
   future.

   The rule is, if an item appears in one state with the singleton "#"
   as its lookahead set, then its lookahead set in the other state
   must contain "#".

   So, either the second lookahead set is also the singleton "#", and
   no end-of-stream conflict exists, or it is larger, and the second
   state already contains an end-of-stream conflict.

   Put another way, we do not want to merge two lookahead sets when one
   contains "#" alone and the other does not contain "#".

   I invented this rule to complement Pager's criterion. I believe,
   but I am not 100% sure, that it does indeed prevent end-of-stream
   conflicts and that it is stable.

   Thanks to Sébastien Hinderer for reporting the bug caused by the
   absence of this extra criterion. *)

let eos_compatible  (k1, tsr1) (k2, tsr2) =
  assert (k1 = k2);
  let n = Array.length tsr1 in
  let rec loop i =
    if i = n then
      true
    else
      let ts1 = tsr1.(i)
      and ts2 = tsr2.(i) in
      begin
        if TerminalSet.mem Terminal.sharp ts1 && TerminalSet.is_singleton ts1 then
          (* "#" is alone in one set: it must be a member of the other set. *)
          TerminalSet.mem Terminal.sharp ts2
        else if TerminalSet.mem Terminal.sharp ts2 && TerminalSet.is_singleton ts2 then
          (* Symmetric condition. *)
          TerminalSet.mem Terminal.sharp ts1
        else
          true
      end
      && loop (i+1)
  in
  loop 0

(* This function determines whether two (core-equivalent) states can
   be merged without creating spurious reductions on the [error]
   token.

   The rule is, we merge two states only if they agree on which
   reductions are permitted on the [error] token.

   Without this restriction, we might end up in a situation where we
   decide to introduce an [error] token into the input stream and
   perform a reduction, whereas a canonical LR(1) automaton,
   confronted with the same input string, would fail normally -- that
   is, it would introduce an [error] token into the input stream, but
   it would not be able to perform a reduction right away: the current
   state would be discarded.

   In the interest of more accurate (or sane, or predictable) error
   handling, I decided to introduce this restriction as of 20110124.
   This will cause an increase in the size of automata for grammars
   that use the [error] token. It might actually make the [error]
   token somewhat easier to use.

   Note that two sets can be in the subsumption relation and still
   be error-incompatible. Error-compatibility requires equality of
   the lookahead sets, restricted to [error].

   Thanks to Didier Rémy for reporting a bug caused by the absence
   of this extra criterion. *)

let error_compatible  (k1, tsr1) (k2, tsr2) =
  assert (k1 = k2);
  let n = Array.length tsr1 in
  let rec loop i =
    if i = n then
      true
    else
      let ts1 = tsr1.(i)
      and ts2 = tsr2.(i) in
      begin
        if TerminalSet.mem Terminal.error ts1 then
          (* [error] is a member of one set: it must be a member of the other set. *)
          TerminalSet.mem Terminal.error ts2
        else if TerminalSet.mem Terminal.error ts2 then
          (* Symmetric condition. *)
          TerminalSet.mem Terminal.error ts1
        else
          true
      end
      && loop (i+1)
  in
  loop 0

(* Union of two states. The two states must have the same core. The
   new state is obtained by pointwise union of the lookahead sets. *)

let union (k1, tsr1) ((k2, tsr2) as s2) =
  assert (k1 = k2);
  let tsr = MArray.leq_join TerminalSet.union tsr1 tsr2 in
  (* If the fresh array [tsr] has the same content as [tsr2],
     then we must return the state [s2], unchanged. *)
  if tsr2 == tsr then s2 else (k1, tsr)

(* Restriction of a state to a set of tokens of interest. Every
   lookahead set is intersected with that set. *)

let restrict ts (k, tsr) =
  k, Array.map (fun tsri ->
    TerminalSet.inter tsri ts
  ) tsr

let reductions_table state =
  List.fold_left (fun reductions (ts, prod) ->
    Reductions.add_reductions prod ts reductions
  ) TerminalMap.empty (reductions state)
