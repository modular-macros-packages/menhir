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
= struct
open Lr1.Lr0.G
open E

(* -------------------------------------------------------------------------- *)

(* This functor transforms an analysis where a stack shape is an array of
   symbols into a simpler analysis where a stack shape is just a height. *)

module Height (S : SHAPE with type shape = Symbol.t array) = struct
  include S
  let show_shape height =
    Printf.sprintf "%d" height
  let node_shape node =
    Array.length (node_shape node)
  let production_shape node =
    Array.length (production_shape node)
  let goto_shape node =
    Array.length (goto_shape node)
end

(* -------------------------------------------------------------------------- *)

(* This functor prints the results of an analysis to an output channel. *)

module Print (S : SHAPE with type node := Lr1.node) = struct

  open Printf
  open S
  open G

  let print_node_shape f =
    Lr1.iter @@ fun node ->
    fprintf f "%s_node_shape(%s) = %s\n"
      (match variant with `Short -> "short" | `Long -> "long")
      (Lr1.print node)
      (show_shape (node_shape node))

  let print_production_shape f =
    Production.iterx @@ fun prod ->
    fprintf f "%s_production_shape(%s) = %s\n"
      (match variant with `Short -> "short" | `Long -> "long")
      (Production.print prod)
      (show_shape (production_shape prod))

  let print_goto_shape _f =
    (* At the moment, this data is not printed. *)
    ()

  let print f =
    print_node_shape f;
    print_production_shape f;
    print_goto_shape f

end

(* -------------------------------------------------------------------------- *)

(* [foreach_source symbol] enumerates the nodes that have an outgoing edge
   labeled [symbol]. *)

let foreach_source symbol yield =
  Lr1.targets symbol |> Lr1.NodeSet.iter @@ fun target ->
  Lr1.predecessors target |> Lr1.NodeSet.iter yield

(* -------------------------------------------------------------------------- *)

(* Compute the shape of the stack, as a vector of symbols, at every node.
   This information is part of the short invariant. *)

module StackSymbolsShort =
  StackSymbols.Short(Lr1)()

(* Then, compute the shape of the stack, as a vector of sets of nodes,
   at every node. *)

(* This information is exploited for several purposes:

   1. If [represent_states] is [false] (which is the default), it is used to
   determine which states must be represented.

   2. The Rocq back-end needs this information.

   For large automata, this computation can be somewhat costly; e.g., about
   1.5 seconds for cca_cpp, a 10,000-state automaton.

   Fortunately, [StackStates.Run] is written in such a way that the analysis
   is performed only if its result is demanded. Thus, when this analysis is
   not necessary, it is skipped. *)

module StackStatesShort =
  StackStates.Run(Lr1)(Height(StackSymbolsShort))

(* TODO *)
let _print_stack_states_short () =
  let module P = Print(StackStatesShort) in
  P.print stderr

(* -------------------------------------------------------------------------- *)

(* Determine which states must be represented, that is, explicitly pushed
   onto the stack. For simplicity, a state is either always represented or
   never represented. More fine-grained strategies, where a single state is
   sometimes pushed onto the stack and sometimes not pushed, depending on
   which outgoing transition is being taken, are conceivable, but quite
   tricky, and probably not worth the trouble.

   The conditions listed here are designed to allow the code back-end
   to perform a case analysis on a state, when needed. Thus, it is
   necessary to have detailed knowledge of the code back-end in order
   to understand why these conditions are necessary and sufficient.
   This is not ideal, but the alternative (which would be to generate
   code first and deduce, by analysis of the code, which states must
   be represented) would be complex as well.

   (1) If two states are liable to appear within a single stack cell,
   then one is represented if and only if the other is represented.
   This ensures that the structure of stacks is known everywhere and
   that we can propose types for stacks.

   (2a) If a state [s] has an outgoing transition along nonterminal
   symbol [nt], and if the [goto] table for symbol [nt] has more than
   one target, then state [s] is represented. (When the [goto] table
   has only one target, this means that all [goto] transitions labeled
   [nt] lead to the same state, so the [goto] function can jump to
   this state without performing a case analysis.)

   (2b) If a state [s] has an outgoing transition labeled [nt], and if
   one of the productions associated with [nt] is non-epsilon and needs
   [beforeendp], then [s] must be represented. This caters for the (rare)
   situation where the code back-end must perform a case analysis of
   the current state in order to peek at the top stack cell. *)

module RepresentedStates () : sig

  (**[represented s] determines whether the state [s] is represented. *)
  val represented : Lr1.node -> bool

end = struct

  (* Data. *)

  let rep : bool UnionFind.point array =
    Lr1.init @@ fun _ ->
    UnionFind.fresh false

  (* Getter. *)

  let represented state : bool UnionFind.point =
    rep.(Lr1.encode state)

  (* Setter. *)

  let represent state =
    UnionFind.set (represented state) true

  (* Enforce condition (1) above. *)

  (* [share shape] ensures that in each cell in the shape [shape],
     all of the states are equi-represented. *)

  let share shape =
    shape |> Array.iter @@ fun states ->
    let dummy = UnionFind.fresh false in
    states |> Lr1.NodeSet.iter @@ fun state ->
    UnionFind.union dummy (represented state)

  let () =
    Lr1.iter @@ fun node ->
    share (StackStatesShort.node_shape node)

  let () =
    Production.iter @@ fun prod ->
    share (StackStatesShort.production_shape prod)

  (* Enforce condition (2a) above. *)

  let () =
    Nonterminal.iter @@ fun nt ->
    let symbol = Symbol.N nt in
    if Lr1.NodeSet.cardinal (Lr1.targets symbol) > 1 then
      foreach_source symbol represent

  (* Enforce condition (2b) above. *)

  let () =
    Production.iterx @@ fun prod ->
    let n = Production.length prod in
    if n > 0 && Action.has_beforeend (Production.action prod) then
      let nt = Production.nt prod in
      let symbol = Symbol.N nt in
      foreach_source symbol represent

  (* Define an accessor. *)

  let represented state : bool =
    UnionFind.get (represented state)

  (* Statistics. *)

  let count =
    Lr1.fold (fun node count ->
      if represented node then count + 1 else count
    ) 0

  let () =
    log "%d out of %d states are represented." count Lr1.n

  (* If requested, show a detailed table of which states are represented. *)

  (* TODO *)
  let _print_represented () =
    Lr1.iter @@ fun node ->
    Printf.eprintf "represented(%s) = %b\n"
      (Lr1.print node) (represented node)

end (* RepresentedStates *)

(* This is the final definition of the function [represented]. *)

(* If [represent_states] is [true] then every state is represented
   and the above computation is skipped. *)

let always _ =
  true

let represented : Lr1.node -> bool =
  if X.represent_states then
    always
  else
    let module RS = RepresentedStates() in
    RS.represented

(* [representeds] is analogous to [represented], except that its argument
   is a set of states, all of which must be equi-represented. *)

(* If [represent_states] is [true] then every state is represented so we
   can return [true] without forcing the suspension [states]. *)

let representeds (states : Lr1.NodeSet.t Lazy.t) =
  X.represent_states ||
  let states = Lazy.force states in
  not (Lr1.NodeSet.is_empty states) &&
  represented (Lr1.NodeSet.choose states)

(* -------------------------------------------------------------------------- *)

(* Machinery for the computation of which symbols must keep track of their
   start or end positions. *)

open Keyword

type variable =
  Symbol.t * where (* WhereStart or WhereEnd *)

module M : Fix.IMPERATIVE_MAPS with type key = variable = struct
  type key = variable
  type 'data t = {
    mutable startp: 'data SymbolMap.t;
    mutable endp:   'data SymbolMap.t;
  }
  open SymbolMap
  let create() =
    { startp = empty; endp = empty }
  let clear m =
    m.startp <- empty; m.endp <- empty
  let add (sym, where) data m =
    match where with
    | WhereStart ->
        m.startp <- add sym data m.startp
    | WhereEnd ->
        m.endp <- add sym data m.endp
    | WhereSymbolStart ->
        assert false
  let find (sym, where) m =
    match where with
    | WhereStart ->
        find sym m.startp
    | WhereEnd ->
        find sym m.endp
    | WhereSymbolStart ->
        assert false
  let iter f m =
    iter (fun sym -> f (sym, WhereStart)) m.startp;
    iter (fun sym -> f (sym, WhereEnd)) m.endp
end

(* -------------------------------------------------------------------------- *)

(* We now determine which positions must be kept track of. For simplicity, we
   do this on a per-symbol basis. That is, for each symbol, either we never
   keep track of position information, or we always do. In fact, we do
   distinguish start and end positions. This leads to computing two sets of
   symbols -- those that keep track of their start position and those that
   keep track of their end position.

   A symbol on the right-hand side of a production must keep track of its
   (start or end) position if that position is explicitly requested by a
   semantic action.

   Furthermore, if the left-hand symbol of a production must keep track of its
   start (resp. end) position, then the first (resp. last) symbol of its
   right-hand side (if there is one) must do so as well. That is, unless the
   right-hand side is empty. *)

(* 2015/11/11. When a production [prod] is reduced, the cell that lies at
   the top of the stack (after the cells that correspond to the production's
   right-hand side have been popped, and before a new cell is pushed onto the
   stack) may be consulted for its end position. This implies that this cell
   must exist and must store an end position! Now, when does this happen?

   In the code back-end, this happens only if [prod] is not an epsilon
   production and only if the current state is not an initial state. Indeed,
   for epsilon productions, we read the register [endp] instead of peeking at
   the top stack cell, and at initial states, we read the register [initp].
   Furthermore, this happens only if the semantic action explicitly mentions
   the keyword [$endpos($0)].

   Now, if this happens, what should we do? If this happens in a state [s]
   whose incoming symbol is [sym], then [sym] must keep track of its end
   position. *)

(* I will say that this is a more sophisticated than I would like. The code
   back-end has been known for its efficiency and I am trying to maintain this
   property -- in particular, I would like to keep track of no positions at all,
   if the user doesn't use any position keyword. But I am suffering. *)

(* If [--represent-positions] is passed on the command line, then this
   computation is skipped and every position is tracked. *)

module TrackPositions () : sig
  val track_startp: Symbol.t -> bool
  val track_endp: Symbol.t -> bool
end = struct

module F =
  LatticeInequalities.Make(M)(Fix.Prop.Boolean)()

let () =

  (* We gather the constraints explained above in a loop over every
     (non-start) production [prod]. *)

  Production.iterx begin fun prod ->

    let nt, rhs = Production.def prod
    and ids = Production.identifiers prod
    and action = Production.action prod in
    let length = Array.length rhs in

    if length > 0 then begin

      (* A non-epsilon production. *)

      (* If [nt] keeps track of its start position, then the first symbol
         in the right-hand side must do so as well. *)
      F.edge (Symbol.N nt, WhereStart) (rhs.(0), WhereStart);
      (* If [nt] keeps track of its end position, then the last symbol
         in the right-hand side must do so as well. *)
      F.edge (Symbol.N nt, WhereEnd) (rhs.(length - 1), WhereEnd)

    end;

    (* Examine the production's position keywords. *)

    Action.keywords action |> KeywordSet.iter begin function
      | Position (Before, _, _) ->
          (* See the long comment above (2015/11/11). The condition is as
             follows: if [prod] refers to [$endpos($0)], if the state [s]
             carries an outgoing transition labeled [nt], if the incoming
             symbol of [s] is [sym], and if [prod] is not an epsilon
             production, then [sym] must keep track of its end position. *)
          if length > 0 then
            foreach_source (Symbol.N nt) @@ fun s ->
            Lr1.incoming_symbol s |> Option.iter @@ fun sym ->
            F.source true (sym, WhereEnd)
      | Position (Left, _, _) ->
          (* [$startpos] and [$endpos] have been expanded away. *)
          assert false
      | Position (_, _, FlavorLocation) ->
          (* [$loc] and [$sloc] have been expanded away. *)
          assert false
      | Position (RightNamed _, WhereSymbolStart, _) ->
          (* [$symbolstartpos(x)] does not exist. *)
          assert false
      | Position (RightNamed id, where, _) ->
          (* If the semantic action mentions [$startpos($i)], then the
             [i]-th symbol in the right-hand side must keep track of
             its start position. Similarly for end positions. *)
          Array.iteri (fun i id' ->
            if id = id' then
              F.source true (rhs.(i), where)
          ) ids
    end (* end of loop on keywords *)

  end (* end of loop on productions *)

let track : variable -> bool option =
  let module S = F.Solve() in
  S.solution

let track : variable -> bool =
  fun x -> Option.value (track x) ~default:false

let track_startp symbol =
  track (symbol, WhereStart)

let track_endp symbol =
  track (symbol, WhereEnd)

let sum_over_every_symbol (f : Symbol.t -> bool) : int =
  let c = ref 0 in
  Symbol.iter (fun sym -> if f sym then c := !c + 1);
  !c

let () =
  log
    "%d out of %d symbols keep track of their start position.\n\
     %d out of %d symbols keep track of their end position.\n"
    (sum_over_every_symbol track_startp) (Terminal.n + Nonterminal.n)
    (sum_over_every_symbol track_endp) (Terminal.n + Nonterminal.n)

end (* TrackPositions *)

let track_startp, track_endp =
  if X.represent_positions then
    always, always
  else
    let module T = TrackPositions() in
    T.track_startp, T.track_endp

(* -------------------------------------------------------------------------- *)
(* Constructors and accessors for information about the stack. *)

(* Types. *)

type cell = {
  symbol: Symbol.t;
  states: Lr1.NodeSet.t Lazy.t;
  holds_semv: bool;
  holds_state: bool;
  holds_startp: bool;
  holds_endp: bool;
}

type shape =
  cell array

(* Constructors. *)

let empty : shape =
  [||]

(* If [--represent-values] is passed on the command line, then every semantic
   value is stored. *)

let has_semv symbol =
  X.represent_values ||
  match symbol with
  | Symbol.N _nt ->
      true
  | Symbol.T tok ->
      match Terminal.ocamltype tok with
      | None ->
          (* Token has unit type and is omitted in stack cell. *)
          false
      | Some _ocamltype ->
          true

let cell symbol states =
  let holds_semv = has_semv symbol in
  let holds_state = representeds states in
  let holds_startp, holds_endp = track_startp symbol, track_endp symbol in
  { symbol; states; holds_semv; holds_state; holds_startp; holds_endp }

(* Accessors. *)

let symbol cell =
  cell.symbol

let states cell =
  Lazy.force cell.states

let holds_semv cell =
  cell.holds_semv

let holds_state cell =
  cell.holds_state

let holds_startp cell =
  cell.holds_startp

let holds_endp cell =
  cell.holds_endp

let present cell =
  cell.holds_state || cell.holds_semv || cell.holds_startp || cell.holds_endp

let similar cell1 cell2 =
  Symbol.equal cell1.symbol cell2.symbol &&
  cell1.holds_state = cell2.holds_state
    (* The fields [holds_semv], [holds_startp] and [holds_endp]
       do not need to be compared, because they are determined
       by the field [symbol]. The field [states] does not need
       to be compared because it does not influence the layout
       of the cell; comparing the field [holds_state] suffices. *)

(* TODO
Caveat: all [states] fields are ignored in this computation;
     the [states] fields of the result are not as accurate as they could be, and
     some opportunities to return [None], based on an inspection of the [states]
     fields, are missed. *)

let meet w1 w2 =
  let n1, n2 = Array.length w1, Array.length w2 in
  let n = min n1 n2 in
  let suffix1, suffix2 = MArray.suffix w1 n, MArray.suffix w2 n in
  if MArray.for_all2 similar suffix1 suffix2 then
    (* [w1] and [w2] agree on their common suffix. The meet is
       then the longest of the two shapes. (We could compute the
       intersection of the [states] fields, but we assume that
       the caller is not interested in this information.) *)
    Some (if n1 < n2 then w2 else w1)
  else
    (* [w1] and [w2] disagree on their common suffix. This implies
        that their meet is bottom. *)
    None

let push, pop, top, filter =
  MArray.(push, pop, last, filter)

let get, length, fold_left, append, to_list =
  Array.(get, length, fold_left, append, to_list)

let split w k =
  if k = 0 then
    (* A fast path for the most common case. *)
    w, [||]
  else
    let n = Array.length w in
    assert (0 <= k && k <= n);
    let lower = Array.sub w 0 (n - k) in
    let upper = MArray.suffix w k in
    lower, upper

let show_shape w =
  if length w > 0 then
    Symbol.print_array' false (Array.map symbol w)
  else
    " <empty>"

(* -------------------------------------------------------------------------- *)

(* Publish the short invariant. *)

(* Suppose we have a function [symbols] that maps things to vectors of
   symbols and a function [states] that maps things to vectors of sets of
   states. Then, we want to construct and tabulate a function that maps
   things to vectors of cells. *)

let publish tabulate symbols states =
  tabulate @@ fun thing ->
  if X.represent_states then
    (* In this special case, all states are represented, so the vectors
       [states] and [symbols] must have the same length; no truncation
       is necessary. Furthermore, we want to avoid evaluating [states]
       because that would cause the on-demand analysis to run. *)
    let symbols = symbols thing in
    let k = Array.length symbols in
    Array.init k @@ fun i ->
    cell symbols.(i) (lazy (states thing).(i))
  else
    let symbols, states = symbols thing, states thing in
    assert (Array.length symbols >= Array.length states);
    (* We allow [states] to be shorter than [symbols]. This is required in the
       computation of the long invariant, where [validate] can reject sets of
       states that are not equi-represented. In that case, we truncate
       [symbols] to match [states]. *)
    let k = Array.length states in
    let symbols = MArray.suffix symbols k in
    Array.init k @@ fun i ->
    cell symbols.(i) (lazy states.(i))

module Short = struct

  open MLazy

  module G = Lr1.Lr0.G

  let variant = `Short

  let node_shape : Lr1.node -> shape =
    publish (lazily Lr1.tabulate)
      StackSymbolsShort.node_shape
      StackStatesShort.node_shape

  let production_shape : Production.t -> shape =
    publish (lazily Production.tabulate)
      StackSymbolsShort.production_shape
      StackStatesShort.production_shape

  let goto_shape : Nonterminal.t -> shape =
    publish (lazily Nonterminal.tabulate)
      StackSymbolsShort.goto_shape
      StackStatesShort.goto_shape

  let show_shape =
    show_shape

end

(* -------------------------------------------------------------------------- *)

(* Compute and publish the long invariant. *)

(* Fortunately, all of the building blocks are at hand, so this is easy. *)

(* A caveat: it is not obvious that the sets of states computed here are
   equi-represented. (A set is equi-represented if all of its elements are
   represented *or* all of its elements are unrepresented.) Yet, we need
   this property, otherwise the long invariant cannot be safely translated
   to an OCaml GADT.

   One might think that this property is likely true, because every set of
   states that appears somewhere in the long invariant must also appear
   somewhere in the short invariant, and we know that every set of states in
   the short invariant is equi-represented, because we have explicitly
   imposed this requirement. However, this is *incorrect*: testing shows
   that not every set of states in the long invariant is equi-represented.

   To work around this problem, we truncate the long invariant so as to
   forget about any stack cells that are not equi-represented. *)

module Long = struct

  module G = Lr1.Lr0.G

  (* Compute. *)

  module StackSymbolsLong =
    StackSymbols.Long(Lr1)()

  module StackStatesLong =
    StackStates.Run(Lr1)(Height(StackSymbolsLong))

  (* TODO *)
  let _print_stack_states_long () =
    let module P = Print(StackStatesLong) in
    P.print stderr

  (* Validate. *)

  let unrepresented node =
    not (represented node)

  let equi_represented nodes =
    Lr1.NodeSet.for_all represented nodes ||
    Lr1.NodeSet.for_all unrepresented nodes

  let validate states =
    MArray.greatest_suffix_forall equi_represented states

  let validate states =
    if X.represent_states then
      (* If every state must be represented, then every set of states is
         equi-represented, so validation always succeeds. *)
      states
    else
      validate states

  let node_shape s =
    validate @@ StackStatesLong.node_shape s

  let production_shape prod =
    validate @@ StackStatesLong.production_shape prod

  let goto_shape nt =
    validate @@ StackStatesLong.goto_shape nt

  (* Publish. *)

  let variant = `Long

  let node_shape : Lr1.node -> shape =
    publish Lr1.tabulate
      StackSymbolsLong.node_shape
      node_shape

  let production_shape : Production.t -> shape =
    publish Production.tabulate
      StackSymbolsLong.production_shape
      production_shape

  let goto_shape : Nonterminal.t -> shape =
    publish Nonterminal.tabulate
      StackSymbolsLong.goto_shape
      goto_shape

  let show_shape =
    show_shape

end (* Long *)

(* -------------------------------------------------------------------------- *)

end (* Run *)
