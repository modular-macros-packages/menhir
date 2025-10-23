(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Attribute
open PlainSyntax
open PlainSyntaxAccessors

module Make (G : sig
  val grammar: grammar
end) () = struct

open G

type ocamltype = OCamlType.ocamltype =
  | Declared of string located
  | Inferred of string

(* -------------------------------------------------------------------------- *)

(* [index] indexes a list of (distinct) strings, that is, assigns an
   integer index to each string and builds mappings both ways between
   strings and indices. *)

(* The order of the list is respected: the [i]-th element of the list
   [strings] receives the index [i]. *)

let index (strings : string list) : int * string array * int StringMap.t =
  let name = Array.of_list strings
  and n, map = List.fold_left (fun (n, map) s ->
    n+1, StringMap.add s n map
  ) (0, StringMap.empty) strings in
  assert (StringMap.cardinal map = n);
  n, name, map

(* -------------------------------------------------------------------------- *)

(* Nonterminal symbols. *)

module Nonterminal = struct

  type t = int

  let equal (nt1 : t) (nt2 : t) =
    nt1 = nt2

  let compare = (-)

  (* Determine how many nonterminals we have, and build mappings both ways
     between names and indices. For each user start symbol [S], an internal
     start symbol [S'] is created. No name collision is possible because the
     quote character ['] cannot occur in the name of a symbol. *)

  let new_start_nonterminals =
    StringSet.fold (fun symbol ss -> (symbol ^ "'") :: ss) grammar.start_symbols []

  let original_nonterminals =
    nonterminals grammar

  let start =
    List.length new_start_nonterminals

  let (n : int), (name : string array), (map : int StringMap.t) =
    index (new_start_nonterminals @ original_nonterminals)

  let is_internal_start nt =
    nt < start

  let is_user_start nt =
    StringSet.mem name.(nt) grammar.start_symbols

  let n2i i = i

  let i2n i =
    assert (0 <= i && i < n);
    i

  let lookup symbol =
    StringMap.find symbol map

  let print normalize nt =
    if normalize then
      MString.normalize name.(nt)
    else
      name.(nt)

  let[@inline] init f =
    Array.init n f

  let tabulate f =
    Array.get (init f)

  let[@inline] iter f =
    MInt.iteri n f

  let[@inline] fold f accu =
    MInt.foldi n f accu

  let[@inline] map f =
    MInt.mapi n f

  let[@inline] iterx f =
    MInt.iterij start n f

  let[@inline] foldx f accu =
    MInt.foldij start n f accu

  let[@inline] mapx f =
    MInt.mapij start n f

  let ocamltype : t -> ocamltype option =
    tabulate @@ fun nt ->
    try
      Some (StringMap.find name.(nt) grammar.types)
    with Not_found ->
      None

  let ocamltype nt =
    assert (not (is_internal_start nt));
    ocamltype nt

  let ocamltype_of_start_symbol nt =
    Option.get (ocamltype nt)

  let symbols_without_ocamltype () =
    foldx (fun nt accu ->
      match ocamltype nt with
      | Some _ ->
          accu
      | None ->
          nt :: accu
    ) []

  let positions : t -> ranges =
    tabulate @@ fun nt ->
    if is_internal_start nt then []
    else (StringMap.find name.(nt) grammar.rules).positions

  let attributes : attributes array =
    Array.make n []

  let () =
    grammar.rules |> StringMap.iter @@ fun symbol rule ->
    let nt = lookup symbol in
    attributes.(nt) <- rule.attributes

  let attributes nt =
    attributes.(nt)

  let[@inline] iter_internal_start f =
    MInt.iteri start f

  (* Build a list of the user start symbols and sort it,
     so that [iter_user_start] produces elements in the
     correct order. *)

  let user_start_symbols =
    grammar.start_symbols
    |> StringSet.elements
    |> List.map lookup
    |> List.sort Int.compare

  let iter_user_start f =
    List.iter f user_start_symbols

end

(* Sets and maps of nonterminal symbols. *)

module NonterminalMap =
  IntMap.Make(Nonterminal)()

module NonterminalSet =
  NonterminalMap.Domain

(* -------------------------------------------------------------------------- *)

(* Terminal symbols. *)

module Terminal = struct

  type t = int

  let compare = (-)

  let equal (t1 : t) (t2 : t) =
    t1 = t2

  (* Determine how many terminals we have and build mappings
     both ways between names and indices. A new terminal "#"
     is created. A new terminal "error" is created. The fact
     that the integer code assigned to the terminal "#"
     is the last one is exploited in the table-based back-end.
     (The right-most row of the action table is not created.)

     Pseudo-tokens (used in %prec declarations, but never
     declared using %token) are filtered out. *)

  (* In principle, the number of the [error] token is irrelevant.
     It is currently 0, but we do not rely on that. *)

  let (n : int), (name : string array), (map : int StringMap.t) =
    index ("error" :: tokens grammar @ [ "#" ])

  let t2i i = i

  let i2t i =
    assert (0 <= i && i < n);
    i

  let lookup name =
    StringMap.find name map

  let print t =
    name.(t)

  let sharp =
    lookup "#"

  let error =
    lookup "error"

  let special t =
    t = sharp || t = error

  let real t =
    t <> sharp && t <> error

  let eof =
    try
      Some (lookup "EOF")
    with Not_found ->
      None

  let default = (* applicable to [error] and [#] *)
    {
      tk_precedence    = UndefinedPrecedence;
      tk_associativity = UndefinedAssoc;
      tk_ocamltype     = None;
      tk_is_declared   = true;
      tk_position      = Range.dummy;
      tk_attributes    = [];
      tk_alias         = None;
    }

  let properties =
    Array.init n @@ fun t ->
    try
      StringMap.find name.(t) grammar.tokens
    with Not_found ->
      assert (special t);
      default

  let ocamltype t =
    properties.(t).tk_ocamltype

  let associativity t =
    properties.(t).tk_associativity

  let attributes t =
    properties.(t).tk_attributes

  let alias t =
    properties.(t).tk_alias

  let unquoted_alias t =
    alias t
    |> Option.map MString.unquote

  let init f =
    Array.init n f

  let[@inline] iter f =
    MInt.iteri n f

  let[@inline] fold f accu =
    MInt.foldi n f accu

  let[@inline] map f =
    MInt.mapi n f

  let () =
    assert (sharp = n - 1)

  let[@inline] iterx f =
    MInt.iteri sharp f

  let[@inline] foldx f accu =
    MInt.foldi sharp f accu

  let[@inline] mapx f =
    MInt.mapi sharp f

  let () =
    assert (error = 0)

  let[@inline] iter_real f =
    MInt.iterij 1 sharp f

  let[@inline] fold_real f accu =
    MInt.foldij 1 sharp f accu

  let[@inline] map_real f =
    MInt.mapij 1 sharp f

  let tokens_without_an_alias =
    let accu = ref 0 in
    let () =
      iter_real @@ fun t ->
      if alias t = None then
        incr accu
    in
    !accu

  let every_token_has_an_alias =
    tokens_without_an_alias = 0

end

(* -------------------------------------------------------------------------- *)

(* Maps of terminal symbols. *)

module TerminalMap =
  IntMap.Make(Terminal)()

(* Sets of terminal symbols are used intensively in the LR(1) construction,
   so it is important that they be as efficient as possible. *)

module TerminalSet = struct

  include TerminalMap.Domain

  let universe =
    Terminal.fold_real add empty

  (* The signature [Bitsets.API.SET] does not offer [of_list],
     so we implement this function here in a naive way. *)

  let of_list xs =
    List.fold_right add xs empty

  let print ts =
    MString.separated_iter Terminal.print " " (fun f -> iter f ts)

  (* The following definitions aim to define [identify]. *)

  (* [big_max f ts] computes the maximum of the function [f] over the
     set of terminal symbols [ts]. *)

  let big_max (f : elt -> int) (ts : t) : int =
    fold (fun t accu -> max (f t) accu) ts 0

  (* [separator] is a run of underscore characters '_' that is long
     enough so as to guarantee that no terminal symbol contains such
     a run in its name. *)

  let separator : string Lazy.t =
    lazy begin
      let longest_run t = MString.longest_run '_' (Terminal.print t) in
      let k = big_max longest_run universe in
      String.make (k+1) '_'
    end

  (* [identify ts] converts the set of terminal symbols [ts] into
     a valid OCaml identifier. The cardinal of the set is mentioned
     (for informative purposes; it is not required for disambiguation)
     and the symbols are separated using [separator]. *)

  let identify ts =
    String.concat (Lazy.force separator) (
      (* The cardinal: *)
      Printf.sprintf "c%d" (cardinal ts) ::
      (* The symbols: *)
      List.map Terminal.print (elements ts)
    )

  (* The following definitions are used in the computation of FIRST sets
     below. They are not exported outside of this file. *)

  type property =
    t

  let bottom =
    empty

  let is_maximal _ =
    false

  let leq_join =
    union

end

(* -------------------------------------------------------------------------- *)

(* Symbols. *)

module Symbol = struct

  type t =
    | N of Nonterminal.t
    | T of Terminal.t

  let compare sym1 sym2 =
    match sym1, sym2 with
    | N nt1, N nt2 ->
        Nonterminal.compare nt1 nt2
    | T t1, T t2 ->
        Terminal.compare t1 t2
    | N _, T _ ->
        1
    | T _, N _ ->
        -1

  let[@inline] equal sym1 sym2 =
    compare sym1 sym2 = 0

  let[@inline] is_terminal sym =
    match sym with N _ -> false | T _ -> true

  let[@inline] is_nonterminal sym =
    match sym with T _ -> false | N _ -> true

  let lookup name =
    try
      T (Terminal.lookup name)
    with Not_found ->
      try
        N (Nonterminal.lookup name)
      with Not_found ->
        assert false

  let iter f =
    Terminal.iter (fun t -> f (T t));
    Nonterminal.iter (fun nt -> f (N nt))

  let print normalize sym =
    match sym with
    | N nt ->
        Nonterminal.print normalize nt
    | T t ->
        Terminal.print t

  let non_error sym =
    match sym with
    | T t ->
        t <> Terminal.error
    | N _ ->
        true

  let print_array_bullet normalize dot symbols =
    MString.subarray_bullet (print normalize) " " 0 dot "." symbols

  let print_subarray normalize start symbols =
    MString.subarray_bullet (print normalize) " " start (-1) "." symbols

  let print_array normalize symbols =
    print_subarray normalize 0 symbols

  let print_list normalize symbols =
    print_array normalize (Array.of_list symbols)

end

(* -------------------------------------------------------------------------- *)

(* Sets of symbols. *)

module SymbolSet = struct

  include Set.Make(Symbol)

  let print normalize symbols =
    Symbol.print_list normalize (elements symbols)

  (* The following definitions are used in the computation of symbolic FOLLOW
     sets below. They are not exported outside of this file. *)

  type property =
    t

  let bottom =
    empty

  let leq =
    subset

  let join =
    union

end

(* -------------------------------------------------------------------------- *)

(* Maps of symbols. *)

module SymbolMap = struct

  include Map.Make(Symbol)

  let domain m =
    fold (fun symbol _ accu ->
      symbol :: accu
    ) m []

  let init f xs =
    List.fold_left (fun accu x ->
      add x (f x) accu
    ) empty xs

end

(* -------------------------------------------------------------------------- *)

(* Productions. *)

module Production = struct

  type index =
      int
  type t = index

  (* For every start symbol [S], a new production S' -> S is created.
     It is known as a start production. *)

  (* Count how many productions we have, including the start productions.
     This is [n]. *)

  let n : int =
    let n = StringMap.fold (fun _ { branches = branches } n ->
      n + List.length branches
    ) grammar.rules 0 in
    n + StringSet.cardinal grammar.start_symbols

  let p2i prod =
    prod

  let i2p prod =
    assert (prod >= 0 && prod < n);
    prod

  (* Create a number of uninitialized tables that map a production index to
     information about this production. *)

  (* [table] maps a production to the left-hand side and right-hand side of
     this production. [identifiers] maps a production to an array of the
     identifiers that are used to name the elements of the right-hand side.
     [actions] maps a production to an optional semantic action. (Only the
     start productions have none.) [positions] maps a production to an array
     of the positions (in the .mly file) of the elements of the right-hand
     side. [rhs_attributes] maps a production to an array of the attributes
     attached to the elements of the right-hand side. [prec_decl] maps a
     production to an optional [%prec] annotation. [production_level] maps a
     production to a production level. [attributes] maps a production to the
     list of its attributes. *)

  let table : (Nonterminal.t * Symbol.t array) array =
    Array.make n (-1, [||])

  let identifiers : identifier array array =
    Array.make n [||]

  let actions : action option array =
    Array.make n None

  let positions : Range.range list array =
    Array.make n []

  let rhs_attributes : attributes array array =
    Array.make n [||]

  let prec_decl : symbol located option array =
    Array.make n None

  let production_level : production_level array =
    (* The start productions receive a level that pretends that they
       originate in a fictitious "builtin" file. So, a reduce/reduce
       conflict that involves a start production will not be solved. *)
    let dummy = ProductionLevel (InputFile.builtin, 0) in
    Array.make n dummy

  let attributes : attributes array =
    Array.make n []

  (* [ntprods] maps a nonterminal symbol to the interval of its productions. *)

  let ntprods : (int * int) array =
    Array.make Nonterminal.n (-1, -1)

  (* Create the start productions, populating the above arrays as appropriate.
     [start] is the number of start productions, therefore also the index of
     the first non-start production. [startprods] is a mapping of the start
     symbols to the corresponding start productions. *)

  let (start : int), (startprods : index NonterminalMap.t) =
    StringSet.fold (fun nonterminal (k, startprods) ->
      let nt = Nonterminal.lookup nonterminal
      and nt' = Nonterminal.lookup (nonterminal ^ "'") in
      table.(k) <- (nt', [| Symbol.N nt |]);
      identifiers.(k) <- [| "_1" |];
      ntprods.(nt') <- (k, k+1);
      positions.(k) <- Nonterminal.positions nt;
        (* A start production has no attributes. *)
      k+1,
      NonterminalMap.add nt k startprods
    ) grammar.start_symbols (0, NonterminalMap.empty)

  let[@inline] is_start prod =
    prod < start

  let start_production nt =
    try
      NonterminalMap.find nt startprods
    with Not_found ->
      assert false (* [nt] is not a start symbol *)

  (* Create the non-start productions, populating the above arrays. *)

  let prod_symbol producer =
    Symbol.lookup producer.prod_symbol

  let (_ : int) = StringMap.fold (fun nonterminal { branches } k ->
    let nt = Nonterminal.lookup nonterminal in
    let k' = MList.accumulate k branches @@ fun k branch ->
      let producers = Array.of_list branch.producers in
      let rhs = Array.map prod_symbol producers in
      table.(k) <- (nt, rhs);
      identifiers.(k) <- Array.map prod_id producers;
      actions.(k) <- Some branch.action;
      rhs_attributes.(k) <- Array.map prod_attributes producers;
      production_level.(k) <- branch.production_level;
      prec_decl.(k) <- branch.prec_annotation;
      positions.(k) <- [ branch.branch_position ];
      attributes.(k) <- branch.br_attributes;
      k+1
    in
    ntprods.(nt) <- (k, k');
    k'
  ) grammar.rules start

  (* Accessors. *)

  let[@inline] def prod =
    table.(prod)

  let[@inline] nt prod =
    let nt, _ = table.(prod) in
    nt

  let[@inline] rhs prod =
    let _, rhs = table.(prod) in
    rhs

  let[@inline] length prod =
    Array.length (rhs prod)

  let[@inline] identifiers prod =
    identifiers.(prod)

  let action prod =
    match actions.(prod) with
    | Some action ->
        action
    | None ->
        (* Start productions have no action. *)
        assert (is_start prod);
        assert false

  let[@inline] positions prod =
    positions.(prod)

  let[@inline] attributes prod =
    attributes.(prod)

  let[@inline] rhs_attributes prod =
    rhs_attributes.(prod)

  let[@inline] prec_decl k =
    prec_decl.(k)

  let[@inline] production_level k =
    production_level.(k)

  let classify prod =
    if is_start prod then
      match (rhs prod).(0) with
      | Symbol.N nt ->
          Some nt
      | Symbol.T _ ->
          assert false
    else
      None

  let error_free prod =
    MArray.for_all Symbol.non_error (rhs prod)

  (* Printing a production. *)

  let print prod =
    assert (not (is_start prod));
    let nt, rhs = table.(prod) in
    let nt = Nonterminal.print false nt in
    if Array.length rhs = 0 then
      (* Avoid producing a trailing space. *)
      Printf.sprintf "%s ->" nt
    else
      Printf.sprintf "%s -> %s" nt
        (Symbol.print_subarray false 0 rhs)

  let describe gerund prod =
    match classify prod with
    | Some nt ->
        let ending = if gerund then "ing" else "" in
        Printf.sprintf "accept%s %s" ending (Nonterminal.print false nt)
    | None ->
        let ending = if gerund then "ing" else "e" in
        Printf.sprintf "reduc%s production %s" ending (print prod)

  (* Iteration. *)

  let init f =
    Array.init n f

  let[@inline] iter f =
    MInt.iteri n f

  let[@inline] fold f accu =
    MInt.foldi n f accu

  let[@inline] map f =
    MInt.mapi n f

  let[@inline] iterx f =
    MInt.iterij start n f

  let[@inline] foldx f accu =
    MInt.foldij start n f accu

  let[@inline] mapx f =
    MInt.mapij start n f

  let[@inline] iternt nt f =
    let k, k' = ntprods.(nt) in
    MInt.iterij k k' f

  let[@inline] foldnt nt f accu =
    let k, k' = ntprods.(nt) in
    MInt.foldij k k' f accu

  let[@inline] mapnt nt f =
    let k, k' = ntprods.(nt) in
    MInt.mapij k k' f

  let foldnt_lazy nt f accu =
    let k, k' = ntprods.(nt) in
    MInt.foldij_lazy k k' f accu

  let[@inline] tabulate f =
    MArray.tabulate n f

end

(* -------------------------------------------------------------------------- *)

(* Maps of productions. *)

module ProductionMap = struct

  include Patricia

  (* Iteration over the start productions only. *)

  let[@inline] start f =
    MInt.foldi Production.start (fun prod m ->
      add prod (f prod) m
    ) empty

end

(* -------------------------------------------------------------------------- *)

(* Properties of the grammar. *)

let attributes =
  grammar.gr_attributes

let grammar_uses_error_token () =
  let used = ref false in
  let () =
    Production.iterx @@ fun prod ->
    if not (Production.error_free prod) then
      used := true
  in
  !used

(* -------------------------------------------------------------------------- *)

(* Many analyses of the grammar are expressed as fixed point computations.
   We exploit the generic fixed point algorithm in [Fix]. *)

(* We perform memoization only at nonterminal symbols. We assume that the
   analysis of a symbol is the analysis of its definition (as opposed to,
   say, a computation that depends on the occurrences of this symbol in
   the grammar). *)

module GenericAnalysis (P : Fix.PROPERTY) (S : sig
  open P

  (* An analysis is specified by the following functions. *)

  (**If [shortcut nt] is [Some p], then the definition of the nonterminal
     symbol [nt] is ignored by the analysis, and this symbol is mapped by
     the analysis to the property [p]. *)
  val shortcut: Nonterminal.t -> property option

  (**[terminal] maps a terminal symbol to a property. *)
  val terminal: Terminal.t -> property

  (**[disjunction] abstracts a binary alternative. That is, when we analyze
     an alternative between several productions, we compute a property for
     each of them independently, then we combine these properties using
     [disjunction]. *)
  val disjunction: property -> (unit -> property) -> property

  (**[conjunction] abstracts a binary sequence. That is, when we analyze a
     sequence, we compute a property for each member independently, then we
     combine these properties using [conjunction]. In general, conjunction
     needs access to the first member of the sequence (a symbol), not just
     to its analysis (a property).

     The property [P.bottom] must be a neutral element for [disjunction]. We
     use [P.bottom] in the analysis of an alternative with zero branches. *)
  val conjunction: Symbol.t -> property -> (unit -> property) -> property

  (**[epsilon] abstracts the empty sequence. It must be a neutral element
     for [conjunction]. *)
  val epsilon: property

  (**The Boolean flag [ignore_error_productions] indicates whether the
     productions that mention the [error] token should be ignored. *)
  val ignore_error_productions: bool

end) : sig
  open P

  (* The results of the analysis take the following form. *)

  (**To every nonterminal symbol, we associate a property. *)
  val nonterminal: Nonterminal.t -> property

  (**To every symbol, we associate a property. *)
  val symbol: Symbol.t -> property

  (**To every suffix of every production, we associate a property.
     The offset [i], which determines the beginning of the suffix,
     must be contained between [0] and [n], inclusive, where [n]
     is the length of the production. *)
  val production: Production.index -> int -> property

end = struct
  open P

  (* The following analysis functions are parameterized by [get], which
     allows making a recursive call to the analysis at a nonterminal symbol.
     [get] maps a nonterminal symbol to a property. *)

  (* Analysis of a symbol. *)

  let symbol sym get : property =
    match sym with
    | Symbol.T t ->
        S.terminal t
    | Symbol.N nt ->
        (* Recursive call to the analysis, via [get]. *)
        get nt

  (* Analysis of (a suffix of) a production [prod], starting at index [i]. *)

  let production prod i get : property =
    assert (not S.ignore_error_productions || Production.error_free prod);
    let rhs = Production.rhs prod in
    let n = Array.length rhs in
    (* Conjunction over all symbols in the right-hand side. This can be viewed
       as a version of [Array.fold_right], which does not necessarily begin at
       index [0]. Note that, because [conjunction] is lazy, it is possible to
       stop early. *)
    let rec loop i =
      if i = n then
        S.epsilon
      else
        let sym = rhs.(i) in
        S.conjunction sym
          (symbol sym get)
          (fun () -> loop (i+1))
    in
    loop i

  (* The analysis is the least fixed point of the following function, which
     analyzes a nonterminal symbol by looking up and analyzing its definition
     as a disjunction of conjunctions of symbols. *)

  let nonterminal nt get : property =
    match S.shortcut nt with
    | Some p ->
        p
    | None ->
        (* Disjunction over all productions for this nonterminal symbol. *)
        P.bottom |> Production.foldnt_lazy nt @@ fun prod rest ->
        if S.ignore_error_productions && not (Production.error_free prod) then
          rest()
        else
          S.disjunction
            (production prod 0 get)
            rest

  (* The least fixed point is taken as follows. It is computed on demand,
     when and where [lfp] is called by the user. *)

  module F =
    Fix.Make
      (Fix.Glue.ArraysAsImperativeMaps(Nonterminal))
      (P)

  let nonterminal =
    F.lfp nonterminal

  (* The auxiliary functions can be published too. *)

  let symbol sym =
    symbol sym nonterminal

  let production prod i =
    production prod i nonterminal

end

(* -------------------------------------------------------------------------- *)

(* Compute which nonterminal symbols are nonempty, that is, which symbols
   generate a nonempty language. Also, compute which nonterminal symbols are
   nullable. The two computations are almost identical. The only difference is
   in the base case: a single terminal symbol is not nullable, but is
   nonempty. *)

module NONEMPTY =
  GenericAnalysis
    (Fix.Prop.Boolean)
    (struct
      let shortcut _nt = None
      (* A terminal symbol is nonempty. *)
      let terminal _ = true
      (* An alternative is nonempty if at least one branch is nonempty. *)
      let disjunction p q = p || q()
      (* A sequence is nonempty if both members are nonempty. *)
      let conjunction _ p q = p && q()
      (* The sequence epsilon is nonempty. It generates the singleton
         language {epsilon}. *)
      let epsilon = true
      let ignore_error_productions = false
    end)

module NULLABLE =
  GenericAnalysis
    (Fix.Prop.Boolean)
    (struct
      let shortcut _nt = None
      (* A terminal symbol is not nullable. *)
      let terminal _ = false
      (* An alternative is nullable if at least one branch is nullable. *)
      let disjunction p q = p || q()
      (* A sequence is nullable if both members are nullable. *)
      let conjunction _ p q = p && q()
      (* The sequence epsilon is nullable. *)
      let epsilon = true
      let ignore_error_productions = false
    end)

(* -------------------------------------------------------------------------- *)

(* Compute FIRST sets. *)

module FIRST =
  GenericAnalysis
    (TerminalSet)
    (struct
      let shortcut _nt = None
      (* A terminal symbol has a singleton FIRST set. *)
      let terminal = TerminalSet.singleton
      (* The FIRST set of an alternative is the union of the FIRST sets. *)
      let disjunction p q = TerminalSet.union p (q())
      (* The FIRST set of a sequence is the union of:
         the FIRST set of the first member, and
         the FIRST set of the second member, if the first member is nullable. *)
      let conjunction symbol p q =
        if NULLABLE.symbol symbol then
          TerminalSet.union p (q())
        else
          p
      (* The FIRST set of the empty sequence is empty. *)
      let epsilon = TerminalSet.empty
      let ignore_error_productions = false
    end)

(* -------------------------------------------------------------------------- *)

(* For every nonterminal symbol [nt], compute a word of minimal length
   generated by [nt]. *)

(* In principle, this analysis subsumes [NONEMPTY] and [NULLABLE]. Indeed,
   [nt] produces a nonempty language if only if the minimal length is finite;
   [nt] is nullable if only if the minimal length is zero. Be careful, though:
   [MINIMAL] analyzes a grammar where error productions are ignored. *)

(* This analysis is in principle more costly than [NONEMPTY] and [NULLABLE].
   In practice, it seems to be very cheap: its cost is not measurable for any
   of the grammars in our benchmark suite. *)

(* This analysis could be made cheaper by computing just the length of a
   minimal sequence, as opposed to a minimal sequence. Currently the sequence
   itself is printed at verbosity level 2 but is otherwise unused. *)

module MINIMAL =
  GenericAnalysis
    (struct
      include CompletedNatWitness
      type property = Terminal.t t
    end)
    (struct
      let shortcut _nt = None
      open CompletedNatWitness
      (* A terminal symbol has length 1. *)
      let terminal = singleton
      (* The length of an alternative is the minimum length of any branch. *)
      let disjunction = min_lazy
      (* The length of a sequence is the sum of the lengths of the members. *)
      let conjunction _ = add_lazy
      (* The epsilon sequence has length 0. *)
      let epsilon = epsilon
      (* The productions that contain [error] are ignored. *)
      let ignore_error_productions = true
    end)

(* -------------------------------------------------------------------------- *)

(* Determine which symbols generate a nonempty word, that is, a word of length
   at least 1. We might tempted to deduce this information from the FIRST sets
   (indeed, a symbol generates a nonempty word if and only if its FIRST set is
   nonempty), but this does not work because (here) we want to ignore error
   productions. *)

(* This information is needed later on by the analysis MAXIMAL. *)

module GENERATES_NONEMPTY_WORD =
  GenericAnalysis
    (Fix.Prop.Boolean)
    (struct
      let shortcut _nt = None
      (* A terminal symbol generates a nonempty word. *)
      let terminal _t = true
      (* An alternative generates a nonempty word if at least one branch
         generates a nonempty word. *)
      let disjunction p q = p || q()
      (* A sequence generates a nonempty word if at least one side generates
         a nonempty word. (We assume both languages are nonempty.) *)
      let conjunction _ p q = p || q()
      (* [epsilon] fails to generate a nonempty word. *)
      let epsilon = false
      (* The productions that contain [error] are ignored. *)
      let ignore_error_productions = true
    end)

let generates_nonempty_word symbol : bool =
  GENERATES_NONEMPTY_WORD.symbol symbol

(* -------------------------------------------------------------------------- *)

(* Compute which symbols can produce words of unbounded length. *)

(* This information is needed later on by the analysis MAXIMAL. *)

(* To do so, we construct the reference graph of the grammar. The vertices
   of this graph are the nonterminal symbols, and, if there is a production
   [A -> alpha B gamma], then there is an edge from A to B. With each such
   edge, we associate a Boolean label, which is [true] if [alpha . gamma]
   generates a nonempty word. *)

module G = struct
  type node = Nonterminal.t
  let n = Nonterminal.n
  let index nt = nt
  let iter = Nonterminal.iter
  let labeled_successors yield nt =
    Production.iternt nt @@ fun prod ->
    let rhs = Production.rhs prod in
    rhs |> Array.iteri @@ fun i symbol ->
    match symbol with
    | Symbol.T _   -> ()
    | Symbol.N nt' ->
        (* There is an edge from [nt] to [nt'], whose Boolean label [gnw]
           is obtained by computing whether the right-hand side, deprived
           of [nt'], can generate a nonempty word. *)
        let gnw =
          rhs |> MArray.existsi @@ fun j symbol ->
          i <> j && generates_nonempty_word symbol
        in
        yield gnw nt'
  let successors yield nt =
    labeled_successors (fun _gnw nt' -> yield nt') nt
end

(* Then, we compute the strongly connected components of the reference
   graph. If a component contains an edge labeled [true], then every
   nonterminal symbol in this component can generate words of unbounded
   length. Mark these symbols in an array [unbounded]. (This computation is
   performed only when needed.) *)

let unbounded : bool array Lazy.t =
  lazy (
    let unbounded = Array.make Nonterminal.n false in
    let module T = Fix.SCC.Run(G) in
    let () =
      (* For each edge of [nt] to [nt'] labeled [gnw], *)
      G.iter @@ fun nt ->
      nt |> G.labeled_successors @@ fun gnw nt' ->
      (* If [gnw] is set and if [nt] and [nt'] lie in the same component, *)
      if gnw && T.representative nt = T.representative nt' then
        (* Then mark every symbol in this component as unbounded. *)
        T.scc (T.representative nt) |> List.iter @@ fun nt ->
        unbounded.(nt) <- true
    in
    unbounded
  )

let unbounded nt : bool =
  (Lazy.force unbounded).(nt)

(* -------------------------------------------------------------------------- *)

(* For every nonterminal symbol [nt], we wish to compute the maximum length of
   a word generated by [nt]. This length can be either finite or [infty]. *)

(* In this analysis, we assume that every symbol generates a nonempty
   language. This removes the need to watch out for productions whose right
   hand side is empty (which we would have to ignore). If this assumption is
   violated then MAXIMAL can produce an over-estimation.

   A similar assumption is made in the computation of FIRST sets above. *)

(* To find out the maximum length of a word generated by each nonterminal
   symbol [nt], we perform a least fixed point computation in the lattice
   [NatMaxInfinity]. Such a computation normally would not terminate, as the
   lattice has unbounded height: a cycle of strictly positive weight in the
   grammar will cause an endless climb. However, we have just identified the
   symbols that participate in these cycles: they are the symbols [nt] such
   that [unbounded nt] is [true]. We use the [shortcut] function to set
   these symbols directly to [infinity]. The cycles that may remain in the
   grammar must have zero weight and cannot cause divergence. The fixed
   point computation must therefore terminate and yield the desired
   information. *)

module MAXIMAL =
  GenericAnalysis
    (NatInfinityMax)
    (struct
      open NatInfinityMax
      let shortcut nt = if unbounded nt then Some infinity else None
      (* A terminal symbol has length 1. *)
      let terminal _t = finite 1
      (* The length of an alternative is the maximum length of any branch. *)
      let disjunction = max_lazy
      (* The length of a sequence is the sum of the lengths of the members. *)
      let conjunction _ = add_lazy
      (* The epsilon sequence has length 0. *)
      let epsilon = bottom
      (* The productions that contain [error] are ignored. *)
      let ignore_error_productions = true
    end)

(* -------------------------------------------------------------------------- *)

(* Computing FOLLOW or symbolic FOLLOW sets is mostly unnecessary for us,
   but a user has requested this analysis. Also, FOLLOW sets happen to be
   useful in the SLR(1) test. *)

(* FOLLOW sets and symbolic FOLLOW sets are computed exactly in the same
   way. To avoid code duplication, we write this computation just once,
   parameterized over a module [P]. The type [P.property] intuitively
   represents a set of symbols. *)

module FOLLOW (P : sig
  include Fix.MINIMAL_SEMI_LATTICE
  val bottom: property
  val terminal: Terminal.t -> property
  val first: Production.index -> int -> property
end) = struct

  module M =
    Fix.Glue.ArraysAsImperativeMaps(Nonterminal)

  module C =
    LatticeInequalities.Make(M)(P)()

  (* Build a system of constraints. *)

  (* Iterate over all start symbols. *)
  let () =
    let sharp = P.terminal Terminal.sharp in
    Nonterminal.iter_internal_start @@ fun nt ->
    (* Add # to FOLLOW(nt). *)
    C.source sharp nt
    (* We need to do this explicitly because our start productions are
       of the form S' -> S, not S' -> S #, so # will not automatically
       appear into FOLLOW(S) when the start productions are examined. *)

  (* Iterate over all productions. *)
  let () =
    Production.iter @@ fun prod ->
    let nt1, rhs = Production.def prod in
    (* Iterate over all nonterminal symbols [nt2] in the right-hand side. *)
    rhs |> Array.iteri @@ fun i symbol ->
    match symbol with
    | Symbol.T _ ->
        ()
    | Symbol.N nt2 ->
        let nullable = NULLABLE.production prod (i+1)
        and first = P.first prod (i+1) in
        (* The FIRST set of the remainder of the right-hand side
           contributes to the FOLLOW set of [nt2]. *)
        C.source first nt2;
        (* If the remainder of the right-hand side is nullable,
           FOLLOW(nt1) contributes to FOLLOW(nt2). *)
        if nullable then
          C.edge nt1 nt2

  (* Second pass. Solve the equations. *)

  module S =
    C.Solve()

  let follow (nt : Nonterminal.t) : P.property =
    Option.value (S.solution nt) ~default:P.bottom

end

(* -------------------------------------------------------------------------- *)

(* Compute standard (concrete) FOLLOW sets. *)

let follow : (Nonterminal.t -> TerminalSet.t) Lazy.t =
  lazy (
    let module F = FOLLOW(struct
      include TerminalSet
      let terminal = singleton
      let first = FIRST.production
    end) in
    F.follow
  )

let follow (nt : Nonterminal.t) : TerminalSet.t =
  (Lazy.force follow) nt

(* -------------------------------------------------------------------------- *)

(* Compute FOLLOW sets for the terminal symbols as well. Again, this is
   unnecessary for us, but has been requested by a user. This is done in
   a single pass over the grammar -- a new fixed point computation is not
   required. *)

let tfollow : TerminalSet.t array Lazy.t =
  lazy (

    let tfollow = Array.make Terminal.n TerminalSet.empty in

    (* Iterate over all productions. *)
    let () =
      Production.iter @@ fun prod ->
      let nt1, rhs = Production.def prod in
      (* Iterate over all terminal symbols [t2] in the right-hand side. *)
      rhs |> Array.iteri @@ fun i symbol ->
      match symbol with
      | Symbol.N _ ->
          ()
      | Symbol.T t2 ->
          let nullable = NULLABLE.production prod (i+1)
          and first = FIRST.production prod (i+1) in
          (* The FIRST set of the remainder of the right-hand side
             contributes to the FOLLOW set of [t2]. *)
          tfollow.(t2) <- TerminalSet.union first tfollow.(t2);
          (* If the remainder of the right-hand side is nullable,
             FOLLOW(nt1) contributes to FOLLOW(t2). *)
          if nullable then
            tfollow.(t2) <- TerminalSet.union (follow nt1) tfollow.(t2)
    in
    tfollow

  )

let tfollow t =
  (Lazy.force tfollow).(t)

(* -------------------------------------------------------------------------- *)

(* Compute symbolic FIRST and symbolic FOLLOW sets. *)

(* The symbolic FIRST set of the word determined by [prod/i] is defined
   (and computed) as follows. *)

let sfirst prod i =
  let rhs = Production.rhs prod in
  let n = Array.length rhs in
  let rec loop i =
    if i = n then
      (* If the word [prod/i] is empty, the set is empty. *)
      SymbolSet.empty
    else
      let sym = rhs.(i) in
      (* If the word [prod/i] begins with a symbol [sym], then [sym]
         itself is part of the symbolic FIRST set, unconditionally. *)
      SymbolSet.union
        (SymbolSet.singleton sym)
        (* Furthermore, if [sym] is nullable, then the symbolic
           FIRST set of the sub-word [prod/i+1] contributes, too. *)
        (if NULLABLE.symbol sym then loop (i + 1) else SymbolSet.empty)
  in
  loop i

(* The symbolic FOLLOW sets are computed just like the FOLLOW sets, except
   we use a symbolic FIRST set instead of a standard FIRST set. *)

let sfollow : (Nonterminal.t -> SymbolSet.t) Lazy.t =
  lazy (
    let module F = FOLLOW(struct
      let bottom = SymbolSet.bottom
      include Fix.Glue.MinimalSemiLattice(SymbolSet)
      let terminal t = SymbolSet.singleton (Symbol.T t)
      let first = sfirst
    end) in
    F.follow
  )

let sfollow (nt : Nonterminal.t) : SymbolSet.t =
  (Lazy.force sfollow) nt

(* -------------------------------------------------------------------------- *)

(* Provide explanations about FIRST sets. *)

(* We want to explain why a terminal symbol [t] appears in the FIRST set of a
   sequence of symbols. Such an explanation involves basic assertions of the
   form: (i) [nt] is nullable and (ii) [t] appears in the FIRST set of [nt].
   We choose to take these basic facts for granted, instead of recursively
   explaining them, so as to keep explanations short. *)

(* We first produce an explanation in abstract syntax, then convert it to a
   human-readable string. *)

type explanation =
  | EObvious
    (**The sequence begins with the desired terminal symbol. *)
  | EFirst of Terminal.t * Nonterminal.t
    (**The sequence begins with a nonterminal symbol
       that produces the desired terminal symbol. *)
  | ENullable of Symbol.t list * explanation
    (**The sequence begins with a list of nullable symbols and ... *)

let explain (t : Terminal.t) (rhs : Symbol.t array) (i : int) =
  let n = Array.length rhs in
  let rec loop i =
    assert (i < n);
    let symbol = rhs.(i) in
    match symbol with
    | Symbol.T t' ->
        assert (Terminal.equal t t');
        EObvious
    | Symbol.N nt ->
        if TerminalSet.mem t (FIRST.nonterminal nt) then
          EFirst (t, nt)
        else begin
          assert (NULLABLE.nonterminal nt);
          match loop (i + 1) with
          | ENullable (symbols, e) ->
              ENullable (symbol :: symbols, e)
          | e ->
              ENullable ([ symbol ], e)
        end
  in
  loop i

let rec convert (e : explanation) : string =
  match e with
  | EObvious ->
      ""
  | EFirst (t, nt) ->
      Printf.sprintf "%s can begin with %s"
        (Nonterminal.print false nt)
        (Terminal.print t)
  | ENullable (symbols, e) ->
      let e = convert e in
      Printf.sprintf "%s can vanish%s%s"
        (Symbol.print_list false symbols)
        (if e = "" then "" else " and ")
        e

(* -------------------------------------------------------------------------- *)
(* Package the analysis results. *)

module Analysis = struct

  let nonempty = NONEMPTY.nonterminal

  let nullable = NULLABLE.nonterminal

  let nullable_symbol = NULLABLE.symbol

  let first = FIRST.nonterminal

  let first_symbol = FIRST.symbol

  (* An initial definition of [nullable_first]. *)

  let nullable_first prod i =
    NULLABLE.production prod i,
    FIRST.production prod i

  (* A memoised version of [nullable_first]. We want to avoid recomputing
     along a production's right-hand side. *)

  let nullable_first =
    Production.tabulate @@ fun prod ->
    MArray.tabulate (Production.length prod + 1) @@ fun i ->
    nullable_first prod i

  let explain_first t rhs i =
    convert (explain t rhs i)

  let follow = follow
  let tfollow = tfollow
  let sfollow = sfollow

  let minimal' nt =
    MINIMAL.nonterminal nt

  let minimal nt =
    CompletedNatWitness.to_int (MINIMAL.nonterminal nt)

  let minimal_prod prod i =
    assert (0 <= i && i <= Production.length prod);
    CompletedNatWitness.to_int (MINIMAL.production prod i)

  let maximal' nt =
    MAXIMAL.nonterminal nt

  let maximal nt =
    NatInfinityMax.to_int (MAXIMAL.nonterminal nt)

  let maximal_prod prod i =
    assert (0 <= i && i <= Production.length prod);
    NatInfinityMax.to_int (MAXIMAL.production prod i)

end

(* -------------------------------------------------------------------------- *)
(* %on_error_reduce declarations. *)

module OnErrorReduce = struct

  (* We keep a [StringMap] internally, and convert back and forth between
     the types [Nonterminal.t] and [string] when querying this map. This
     is not very elegant, and could be changed if desired. *)

  let declarations : Syntax.on_error_reduce_level StringMap.t =
    grammar.on_error_reduce

  let print (nt : Nonterminal.t) : string =
    Nonterminal.print false nt

  let lookup (nt : string) : Nonterminal.t =
    try
      Nonterminal.lookup nt
    with Not_found ->
      (* If this fails, then we have an [%on_error_reduce] declaration
         for an invalid symbol. *)
      assert false

  let reduce prod =
    let nt = Production.nt prod in
    StringMap.mem (print nt) declarations

  let iter f =
    StringMap.iter (fun nt _prec ->
      f (lookup nt)
    ) declarations

  let preferable prod1 prod2 =
    (* The two productions that we are comparing must be distinct. *)
    assert (prod1 <> prod2);
    let nt1 = Production.nt prod1
    and nt2 = Production.nt prod2 in
    (* If they have the same left-hand side (which seems rather unlikely?),
       declare them incomparable. *)
    nt1 <> nt2 &&
    (* Otherwise, look up the priority levels associated with their left-hand
       symbols. *)
    let prec1, prec2 =
      try
        StringMap.find (print nt1) declarations,
        StringMap.find (print nt2) declarations
      with Not_found ->
        (* [preferable] should be used to compare two symbols for which
           there exist [%on_error_reduce] declarations. *)
        assert false
    in
    match production_order prec1 prec2 with
    | Gt ->
        (* [prec1] is a higher integer than [prec2], therefore comes later
           in the file. By analogy with [%left] and friends, we give higher
           priority to later declarations. *)
        true
    | Lt ->
        false
    | Eq
    | Incomparable ->
        (* We could issue a warning or an information message in these cases. *)
        false

end

(* -------------------------------------------------------------------------- *)
(* Facilities for printing sentences. *)

module Sentence = struct

  type sentence =
    Nonterminal.t option * Terminal.t list

  open Printf

  let print_abstract (nto, terminals) : string =
    MString.with_buffer 128 (fun b ->
      Option.iter (fun nt ->
        bprintf b "%s: " (Nonterminal.print false nt)
      ) nto;
      let separator = MString.this_then_that "" " " in
      List.iter (fun t ->
        bprintf b "%s%s" (separator()) (Terminal.print t)
      ) terminals;
      bprintf b "\n";
    )

  let print_concrete (_nto, terminals) : string =
    MString.with_buffer 128 (fun b ->
      let separator = MString.this_then_that "" " " in
      List.iter (fun t ->
        bprintf b "%s%s" (separator()) (Option.get (Terminal.unquoted_alias t))
      ) terminals
    )

  let print style sentence =
    match style with
    | `Abstract ->
        print_abstract sentence
    | `Concrete ->
        print_concrete sentence

end

(* -------------------------------------------------------------------------- *)

end (* module Make *)
