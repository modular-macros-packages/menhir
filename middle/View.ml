(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module defines several signatures that play an important role
   in the middle-end and back-end. *)

include FrontTypes

(* -------------------------------------------------------------------------- *)

(**The signature [GRAMMAR] offers a rich view of a grammar. This view is
   well-suited to the computations that the middle-end performs. It offers
   abstract types of terminal symbols, nonterminal symbols, symbols, and
   productions, as well as efficient sets and maps. Furthermore, it offers
   access to the results of several analyses of the grammar, including
   NONEMPTY, NULLABLE, FIRST, FOLLOW, MINIMAL, MAXIMAL.*)
module type GRAMMAR = sig

(* -------------------------------------------------------------------------- *)

(** {2 Nonterminal Symbols} *)

module Nonterminal : sig

  (**The type of nonterminal symbols includes the nonterminal symbols that
     the user is aware of, plus, for each user start symbol [S], an internal
     start symbol [S'].

     A nonterminal symbol is internally represented as a small nonnegative
     integer. Two conversion functions exist. They are exploited in the
     table-based back-end. They are considered unsafe and should be used as
     little as possible. *)
  type t

  (**[equal] determines whether two nonterminal symbols are equal. *)
  val equal: t -> t -> bool

  (**[compare] compares two nonterminal symbols according to a fixed but
     unspecified total order. *)
  val compare: t -> t -> int

  (**[is_internal_start nt] tells whether the nonterminal symbol [nt] is one
     of the internal start symbols, that is, one of the new symbols [S']
     that are created for each user start symbol [S]. *)
  val is_internal_start: t -> bool

  (**[is_user_start nt] tells whether the nonterminal symbol [nt] is a
     user start symbol, that is, one of the start symbols that the user
     has defined. *)
  val is_user_start: t -> bool

  (**[n] is the total number of nonterminal symbols, including the internal
     start symbols. *)
  val n: int

  (**[n2i] converts a nonterminal symbol to an integer in the range
     [\[0, n)]. *)
  val n2i: t -> int

  (**[i2n] converts an integer in the range [\[0, n)] to a nonterminal
     symbol. This function is {b unsafe} and should be used as little as
     possible. *)
  val i2n: int -> t

  (**[lookup] maps an identifier [s] to a nonterminal symbol. If there is
     no nonterminal symbol named [s] then [Not_found] is raised. *)
  val lookup : string -> t

  (**[print normalize nt] produces a string representation of the
     nonterminal symbol [nt].

     Ideally, [nt] should not be an internal start symbol, as we do not wish
     users to be aware of the existence of these extra nonterminal symbols.
     However, we do sometimes violate this rule when it is difficult to do
     otherwise.

     The Boolean parameter [normalize] indicates whether the string
     representation should be normalized, that is, whether parentheses and
     commas should be replaced with underscores. This is necessary if the
     string is intended for use as a valid nonterminal name or as a valid
     OCaml identifier. *)
  val print: bool -> t -> string

  (**[ocamltype nt] is the OCaml type associated with the nonterminal symbol
     [nt]. [nt] must not be an internal start symbol.

     The value [None] means that the type of this nonterminal symbol is
     unknown. The type of a symbol is known only if the grammar contains a
     %type declaration for it. Type inference augments a grammar with %type
     declarations; see [Infer]. *)
  val ocamltype: t -> ocamltype option

  (**[ocamltype_of_start_symbol] is a simplified variant of [ocamltype],
     which can be applied only to (user) start symbols. Because every start
     has a known type, this function has result type [ocamltype]. *)
  val ocamltype_of_start_symbol: t -> ocamltype

  (**[symbols_without_ocamltype()] returns a list of the nonterminal symbols
     that do not have a known OCaml type. *)
  val symbols_without_ocamltype: unit -> t list

  (**[positions nt] is a list of the positions associated with the definition
     of the nonterminal symbol [nt]. [nt] should not be an internal start
     symbol. There can be more than one position because definitions can be
     split over multiple files. *)
  val positions: t -> ranges

  (**[attributes nt] is the list of attributes attached with the nonterminal
     symbol [nt]. *)
  val attributes: t -> attributes

  (**[init f] initializes a fresh array, which is indexed by the nonterminal
     symbols. In other words, [init] tabulates a function [f] whose domain
     is the nonterminal symbols, producing a table. *)
  val init: (t -> 'a) -> 'a array

  (**[tabulate] tabulates a function [f] whose domain is the nonterminal
     symbols, producing a new function whose time complexity is O(1). *)
  val tabulate: (t -> 'a) -> (t -> 'a)

  (**In every iteration function, the order in which elements are yielded
     corresponds to the integer indices produced by {!n2i}. *)

  (**[iter] enumerates the nonterminal symbols. *)
  val iter: (t -> unit) -> unit

  (**[fold] enumerates the nonterminal symbols. *)
  val fold: (t -> 'a -> 'a) -> 'a -> 'a

  (**[map] enumerates the nonterminal symbols. *)
  val map: (t -> 'a) -> 'a list

  (**[iterx] enumerates the nonterminal symbols
     except the internal start symbols. *)
  val iterx: (t -> unit) -> unit

  (**[foldx] enumerates the nonterminal symbols
     except the internal start symbols. *)
  val foldx: (t -> 'a -> 'a) -> 'a -> 'a

  (**[mapx] enumerates the nonterminal symbols
     except the internal start symbols. *)
  val mapx: (t -> 'a) -> 'a list

  (**[iter_internal_start] enumerates just the internal start symbols. *)
  val iter_internal_start: (t -> unit) -> unit

  (**[iter_user_start] enumerates just the user start symbols. *)
  val iter_user_start: (t -> unit) -> unit

end

(* -------------------------------------------------------------------------- *)

(**Maps whose keys are nonterminal symbols. *)
module NonterminalMap : API.MAP_AND_BITSET with type key = Nonterminal.t

(**Sets whose elements are nonterminal symbols. *)
module NonterminalSet = NonterminalMap.Domain

(* -------------------------------------------------------------------------- *)

(** {2 Terminal Symbols} *)

module Terminal : sig

  (**The type of terminal symbols includes the terminal symbols that the
     user is aware of, plus the two special symbols [#] and [error].

     A terminal symbol is internally represented as a small nonnegative
     integer. Two conversion functions exist. They are exploited in the
     table-based back-end and in other places. They are considered unsafe
     and should be used as little as possible. *)
  type t

  (**[equal] determines whether two terminal symbols are equal. *)
  val equal: t -> t -> bool

  (**[compare] compares two terminal symbols according to a fixed but
     unspecified total order. *)
  val compare: t -> t -> int

  (**[n] is the total number of terminal symbols, including the special
     symbols [#] and [error]. *)
  val n: int

  (**[t2i] converts a terminal symbol to an integer in the range [\[0, n)]. *)
  val t2i: t -> int

  (**[i2t] converts an integer in the range [\[0, n)] to a terminal symbol.
     This function is {b unsafe} and should be used as little as possible. *)
  val i2t: int -> t

  (**[lookup] maps an identifier [s] to a terminal symbol. If there is no
     terminal symbol named [s] then [Not_found] is raised. *)
  val lookup : string -> t

  (**[print t] produces a string representation of the terminal symbol [t]. *)
  val print: t -> string

  (**The special terminal symbol [#] denotes the end of the input stream. *)
  val sharp: t

  (**The special terminal symbol [error] is accessible to the user and is
     used for handling errors. (We discourage its use, but it is currently
     still supported.) *)
  val error: t

  (**[special t] determines whether the terminal symbol [t] is special,
     that is, whether it is one of the special symbols [#] and [error]. *)
  val special: t -> bool

  (**[real t] is equivalent to [not (special t)]. It determines whether
     the terminal symbol is real, that is, whether it is a symbol in the
     input alphabet. *)
  val real: t -> bool

  (**[eof] is the programmer-defined terminal symbol [EOF], if there is one.
     It is recognized based solely on its name, which is fragile, but this
     behavior is documented. This token is assumed to represent [ocamllex]'s
     [eof] pattern. It is used only by the reference interpreter, and in a
     rather non-essential way. *)
  val eof: t option

  (**[ocamltype t] returns the OCaml type of the semantic value of the
     terminal symbol [t]. If this terminal symbol carries no semantic value,
     then it returns [None]. *)
  val ocamltype: t -> ocamltype option

  (**[associativity t] is the associativity of the terminal symbol [t]. It
     plays a role in the resolution of shift/reduce conflicts. *)
  val associativity: t -> associativity

  (**[attributes t] is the list of attributes attached with the terminal
     symbol [t]. *)
  val attributes: t -> attributes

  (**[alias t] is the token alias that has been defined by the user
     for the terminal symbol [t], if there is one. It is a quoted
     escaped string literal.  *)
  val alias: t -> string option

  (**[unquoted_alias t] is the string [alias t],
     deprived of its opening and closing quotes, and unescaped.*)
  val unquoted_alias: t -> string option

  (**[every_token_has_an_alias] is [true] if a token alias has been defined
     by the user for every (real) token. *)
  val every_token_has_an_alias: bool

  (**[init f] initializes a fresh array, which is indexed by the terminal
     symbols. In other words, [init] tabulates a function [f] whose domain
     is the terminal symbols, producing a table. *)
  val init: (t -> 'a) -> 'a array

  (**In every iteration function, the order in which elements are yielded
     corresponds to the integer indices produced by {!t2i}. *)

  (**[iter] enumerates the terminal symbols. *)
  val iter: (t -> unit) -> unit

  (**[fold] enumerates the terminal symbols. *)
  val fold: (t -> 'a -> 'a) -> 'a -> 'a

  (**[map] enumerates the terminal symbols. *)
  val map: (t -> 'a) -> 'a list

  (**[iterx] enumerates the terminal symbols except [#]. *)
  val iterx: (t -> unit) -> unit

  (**[foldx] enumerates the terminal symbols except [#]. *)
  val foldx: (t -> 'a -> 'a) -> 'a -> 'a

  (**[mapx] enumerates the terminal symbols except [#]. *)
  val mapx: (t -> 'a) -> 'a list

  (**[iter_real] enumerates the real terminal symbols, that is,
     the terminal symbols except [#] and [error]. *)
  val iter_real: (t -> unit) -> unit

  (**[fold_real] enumerates the real terminal symbols, that is,
     the terminal symbols except [#] and [error]. *)
  val fold_real: (t -> 'a -> 'a) -> 'a -> 'a

  (**[map_real] enumerates the real terminal symbols, that is,
     the terminal symbols except [#] and [error]. *)
  val map_real: (t -> 'a) -> 'a list

end

(* -------------------------------------------------------------------------- *)

(** {2 Sets and Maps of Terminal Symbols} *)

(**Sets whose elements are terminal symbols. *)
module TerminalSet : sig

  include Bitsets.API.SET with type elt = Terminal.t

  (**[universe] is the set of all real terminal symbols, that is, all terminal
     symbols except [#] and [error]. *)
  val universe: t

  (**[of_list] converts a list of terminal symbols
     into a set of terminal symbols. *)
  val of_list: Terminal.t list -> t

  (**[print] converts a set of terminal symbols into a string representation.
     The symbols are listed one after the other and separated with spaces. *)
  val print: t -> string

  (**[identify] offers a string representation of a set of terminals. This
     string is a valid OCaml identifier and begins with the character 'c'.
     Furthermore, this representation is unambiguous: two distinct sets are
     mapped to two distinct identifiers. *)
  val identify: t -> string

end

(**Maps whose keys are terminal symbols. *)
module TerminalMap : sig
  include API.MAP with type key = Terminal.t
  include API.CONVERT
    with type key := key
     and type 'a map := 'a t
     and type set := TerminalSet.t
end

(* -------------------------------------------------------------------------- *)

(** {2 Symbols} *)

module Symbol : sig

  (**A symbol is either a nonterminal symbol or a terminal symbol. *)
  type t =
    | N of Nonterminal.t
    | T of Terminal.t

  (**[equal] determines whether two symbols are equal. *)
  val equal: t -> t -> bool

  (**[compare] compares two symbols according to a fixed but unspecified
     total order. *)
  val compare: t -> t -> int

  (**[is_terminal sym] determines whether the symbol [sym] is a terminal
      symbol. *)
  val is_terminal: t -> bool

  (**[is_nonterminal sym] determines whether the symbol [sym] is a
     nonterminal symbol. *)
  val is_nonterminal: t -> bool

  (**[lookup] maps an identifier [s] to a symbol. There must be exist a
     nonterminal or terminal symbol named [s]. *)
  val lookup : string -> t

  (**[print normalize sym] prints (produces a string representation of)
     the symbol [sym], using either [Nonterminal.print normalize] or
     [Terminal.print]. *)
  val print: bool -> t -> string

  (**[print_array normalize syms] prints the symbols in the array [syms].
     The symbols are separated with spaces. *)
  val print_array: bool -> t array -> string

  (**[print_subarray normalize start syms] prints the symbols in the array
     [syms], starting at offset [start]. The symbols are separated with
     spaces. *)
  val print_subarray: bool -> int -> t array -> string

  (**[print_array_bullet normalize dot] prints the symbols in the array
     [syms]. The symbols are separated with spaces. Furthermore, if the
     offset [dot] is part of the closed interval [\[start, n\]], where [n]
     is the length of the array [symbols], then the character '.' is
     printed at offset [dot]. Therefore, this character can appear first,
     appear between two symbols, or appear last. *)
  val print_array_bullet: bool -> int -> t array -> string

  (**[iter] enumerates the symbols. *)
  val iter: (t -> unit) -> unit

end

(* -------------------------------------------------------------------------- *)

(** {2 Sets and Maps of Symbols} *)

(**Sets whose elements are symbols. *)
module SymbolSet : sig

  include Set.S with type elt = Symbol.t

  (**[print normalize syms] prints the symbols in the set [syms].
     The symbols are separated with spaces. *)
  val print: bool -> t -> string

end

(**Maps whose keys are symbols. *)
module SymbolMap : sig

  include Map.S with type key = Symbol.t

  (**[terminals m] returns the terminal symbols in the domain of the map [m]. *)
  val terminals: 'a t -> TerminalSet.t

end

(* -------------------------------------------------------------------------- *)

(** {2 Productions} *)

module Production : sig

  (**The type of productions includes the user-defined productions as
     well as the internally generated productions associated with the
     start symbols.

     A production is internally represented as a small nonnegative
     integer. Two conversion functions exist. They are exploited in
     the encoding of items. They are considered unsafe and should
     be used as little as possible. *)
  type index
  type t = index

  (**[compare] compares two symbols according to a fixed but unspecified
     total order. *)
  val compare: t -> t -> int

  (**[n] is the total number of productions, including the start
     productions. *)
  val n: int

  (**[p2i] converts a production to an integer in the range [\[0, n)]. *)
  val p2i: t -> int

  (**[i2p] converts an integer in the range [\[0, n)] to a production. This
     function is {b unsafe} and should be used as little as possible. *)
  val i2p: int -> t

  (**The integer index [start] is published so as to allow the table back-end
     to produce code for [is_start]. It should not be used otherwise. *)
  val start: int

  (**[start_production nt] is the start production associated with the (user)
     nonterminal symbol [nt]. *)
  val start_production: Nonterminal.t -> t

  (**The definition of a production consists of a nonterminal symbol (the
     left-hand side) and an array of symbols (the right-hand side). *)

  (**[def prod] returns the definition of the production [prod]. *)
  val def: t -> Nonterminal.t * Symbol.t array

  (**[nt prod] returns the left-hand side of the production [prod]. *)
  val nt: t -> Nonterminal.t

  (**[rhs prod] returns the right-hand side of the production [prod]. *)
  val rhs: t -> Symbol.t array

  (**[length prod] returns the length of the production [prod].
     It is equivalent to [Array.length (rhs prod)]. *)
  val length: t -> int

  (**[identifiers prod] returns an array of the identifiers that should be used
     to name the semantic values of the symbols in the right-hand side of the
     production [prod]. The length of this array is [length prod]. *)
  val identifiers: t -> identifier array

  (**[action prod] returns the semantic action associated with the production
     [prod]. [prod] must not be a start production. *)
  val action: t -> action

  (**[positions prod] is a list of the positions associated with the
     production [prod]. This is usually a singleton list. A start production
     can have more than one position if the definition of the corresponding
     start symbol is split over multiple files. *)
  val positions: t -> ranges

  (**[attributes prod] are the attributes attached with the production
     [prod]. *)
  val attributes: t -> attributes

  (**[rhs_attributes prod] is an array of the attributes attached with each
     element in the right-hand side of the production [prod]. *)
  val rhs_attributes: t -> attributes array

  (**[prec_decl prod] is the optional %prec annotation carried by the
     production [prod]. This information plays a role in the resolution of
     shift/reduce conflicts. *)
  val prec_decl : t -> prec_annotation

  (**[production_level prod] is the production level of the production [prod].
     This information plays a role in the resolution of reduce/reduce
     conflicts. *)
  val production_level : t -> production_level

  (**[is_start prod] determines whether the production [prod] is a start
     production. *)
  val is_start: t -> bool

  (**[test_start prod] determines whether the production [prod] is a start
     production. If it is a start production, then the user start symbol that
     this production is associated with is returned. If it is a regular
     production, nothing is returned. *)
  val test_start: t -> Nonterminal.t option

  (**[error_free prod] returns [true] if the right-hand side of the
     production [prod] does {i not} contain the [error] token. *)
  val error_free: t -> bool

  (**[print prod] is a string representation of the production [prod]. It
     must not be applied to a start production, because we do not wish
     users to become aware of the existence of these extra productions. *)
  val print: t -> string

  (**[describe prod] is a representation of the production [prod] as a string
     of the form "accepting <nt>" or "reducing <nt> -> <rhs>". The Boolean
     flag [gerund] allows choosing between gerund and infinitive forms. *)
  val describe: bool -> t -> string

  (**In every iteration function, the order in which elements are yielded
     corresponds to the integer indices produced by {!p2i}. *)

  (**[init f] initializes a fresh array, which is indexed by the productions.
     In other words, [init] tabulates a function [f] whose domain is the
     productions, producing a table. *)
  val init: (t -> 'a) -> 'a array

  (**[iter] enumerates the productions. *)
  val iter: (t -> unit) -> unit

  (**[fold] enumerates the productions. *)
  val fold: (t -> 'a -> 'a) -> 'a -> 'a

  (**[map] enumerates the productions. *)
  val map: (t -> 'a) -> 'a list

  (**[iterx] enumerates the productions except the start productions. *)
  val iterx: (t -> unit) -> unit

  (**[foldx] enumerates the productions except the start productions. *)
  val foldx: (t -> 'a -> 'a) -> 'a -> 'a

  (**[mapx] enumerates the productions except the start productions. *)
  val mapx: (t -> 'a) -> 'a list

  (**[iternt] enumerates the productions of the nonterminal symbol [nt]. *)
  val iternt: Nonterminal.t -> (t -> unit) -> unit

  (**[foldnt] enumerates the productions of the nonterminal symbol [nt]. *)
  val foldnt: Nonterminal.t -> (t -> 'a -> 'a) -> 'a -> 'a

  (**[mapnt] enumerates the productions of the nonterminal symbol [nt]. *)
  val mapnt:  Nonterminal.t -> (t -> 'a) -> 'a list

  (**[tabulate] tabulates a function [f] whose domain is the productions,
     producing a new function whose time complexity is O(1). *)
  val tabulate: (t -> 'a) -> (t -> 'a)

end

(* -------------------------------------------------------------------------- *)

(** {2 Maps of Productions} *)

module ProductionMap : sig

  include API.MAP with type key = Production.t

  (**[start] constructs a map whose domain consists of just the start
     productions. *)
  val start: (Production.t -> 'a) -> 'a t

end

(* -------------------------------------------------------------------------- *)

(** {2 Reduction Tables} *)

module Reductions : sig

  (**A reduction table maps terminal symbols to nonempty lists of productions.
     A reduction table concerns just one state. It can be nondeterministic:
     that is, a terminal symbol can be mapped to a list whose length is
     greater than 1. *)
  type t =
    Production.t list TerminalMap.t

  (**A reverse reduction table maps productions to sets of terminal symbols. *)
  type reverse =
    TerminalSet.t ProductionMap.t

  (**[reverse] transforms a reduction table into a reverse reduction table. *)
  val reverse : t -> reverse

  (**[flip] transforms a reverse reduction table into a reduction table. *)
  val flip : reverse -> t

  (**[has_reduce_reduce_conflict reductions] determines whether the reduction
     table [reductions] has a reduce/reduce conflict, that is, whether there
     exists a terminal symbol [t] for which several reductions are
     permitted. *)
  val has_reduce_reduce_conflict: t -> bool

  (**[has_eos_conflict transitions reductions] tells whether a state has an
     end-of-stream conflict, that is, a reduction action on [#] and at least
     one other (shift or reduce) action. We assume that the state's shift and
     goto transitions are described by the table [transitions] and that its
     reductions are described by the reduction table [reductions]. *)
  val has_eos_conflict: _ SymbolMap.t -> t -> bool

end

(* -------------------------------------------------------------------------- *)

(** {2 Items} *)

module Item : sig

  (**An LR(0) item encodes a pair of integers, namely the index of the
     production and the index of the bullet in the production's right-hand
     side. *)
  type t

  (**[equal] determines whether two items are equal. *)
  val equal: t -> t -> bool

  (**[import (prod, i)] converts a pair of a production [prod] and an index [i]
     to an item. The index [i] must lie in the interval [\[0, n\]], where [n]
     is the length of production [prod]. *)
  val import: Production.t * int -> t

  (**[export item] converts the item [item] back to a pair of a production
     [prod] and an index [i]. The index [i] lies in the interval [\[0, n\]],
     where [n] is the length of production [prod]. *)
  val export: t -> Production.t * int

  (**An item is internally encoded as a single integer value. This feature
     should be used as little as possible. It is currently exploited in the
     table back-end only. The decoding function (which is really a copy of
     [export]) is in [InspectionTableInterpreter]. *)
  val marshal: t -> int

  (**[is_start item] determines whether the item [item] is a start item.
     This is the case if the production [prod] is a start production and
     the index [i] is zero. *)
  val is_start: t -> bool

  (**[test_start item] determines whether the item [item] is a start item.
     If so, the user start symbol that corresponds to this start production
     is returned. *)
  val test_start: t -> Nonterminal.t option

  (**[print item] produces a string representation of the item [item]. *)
  val print: t -> string

  (**A shift item is an item where the bullet can still advance. A reduce item
     is an item where the bullet has reached the end of the right-hand side of
     the production. *)
  type kind =
    | Shift of Symbol.t * t
        (**[Shift] carries the (terminal or nonterminal) symbol that is expected
           next and the item that is obtained by advancing the bullet. *)
    | Reduce of Production.t
        (**[Reduce] carries the production that this item is associated with. *)

  (**[classify item] classifies the item [item] as either a shift item or a
     reduce item. *)
  val classify: t -> kind

  (**Maps whose keys are items. These maps are canonical: that is, they
     support the use of OCaml's generic equality and hash functions, so
     they can be used as keys in a hash table. *)
  module Map : API.MAP_AND_SET with type key = t

  (**Sets whose elements are items. These sets are canonical: that is, they
     support the use of OCaml's generic equality and hash functions, so they
     can be used as keys in a hash table. *)
  module Set = Map.Domain

end (* Item *)

(* -------------------------------------------------------------------------- *)

(* Abstract LR states. *)

module AbstractState : sig

  (**An abstract LR state is a map of items to lookahead sets.
     Here, the type of the lookahead sets is undetermined. *)
  type 'lookahead t =
    'lookahead Item.Map.t

  (**[transitions s] computes the outgoing (shift and goto) transitions of the
     state [s], in the form a map of symbols to successor states. The state
     [s] must be closed. The successor states are not necessarily closed. *)
  val transitions: 'lookahead t -> 'lookahead t SymbolMap.t

  (**[reverse_reductions s] computes a reverse reduction table for the state
     [s]. The state [s] must be closed. *)
  val reverse_reductions:  'lookahead t -> 'lookahead ProductionMap.t

end

(* -------------------------------------------------------------------------- *)

(** {2 Properties of the Grammar} *)

(* [attributes] are the attributes attached with the grammar. *)
val attributes: attributes

(**[grammar_uses_error_token()] determines whether the [error] token appears
   in at least one production. *)
val grammar_uses_error_token: unit -> bool

(* -------------------------------------------------------------------------- *)

(** {2 Analyses of the Grammar} *)

module Analysis : sig

  (**[nonempty nt] is true if and only if the nonterminal symbol [nt]
     generates a nonempty language. That is, it is true if and only if
     this symbol generates at least one word. *)
  val nonempty: Nonterminal.t -> bool

  (**[nullable nt] is the NULLABLE flag of the nonterminal symbol [nt].
     That is, it is true if and only if this symbol generates the empty
     word [epsilon]. *)
  val nullable: Nonterminal.t -> bool

  (**[nullable sym] is the NULLABLE flag of the symbol [sym].
     A terminal symbol is not nullable. *)
  val nullable_symbol: Symbol.t -> bool

  (**[first nt] is the FIRST set of the nonterminal symbol [nt]. That is, it
     is the set of terminal symbols with which a word generated by [nt] can
     begin. This analysis is carried out under the assumption that every
     symbol generates a nonempty language. *)
  val first: Nonterminal.t -> TerminalSet.t

  (**[first sym] is the FIRST set of the symbol [sym].
     The FIRST set of a terminal symbol is just itself. *)
  val first_symbol: Symbol.t -> TerminalSet.t

  (**[nullable_first prod i] returns the NULLABLE flag and the FIRST set of
     the production suffix determined by [prod] and [i]. The offset [i] must
     lie in the interval [\[0, n\]], where [n] is the length of production
     [prod]. *)
  val nullable_first: Production.t -> int -> bool * TerminalSet.t

  (**[explain_first t rhs i] explains why the terminal symbol [t] appears in
     the FIRST set of the production suffix determined by [prod] and [i].
     The offset [i] must lie in the interval [\[0, n\]], where [n] is the
     length of production [prod]. *)
  val explain_first: Terminal.t -> Production.t -> int -> string

  (**[follow nt] is the FOLLOW set of the nonterminal symbol [nt]. It is the
     set of terminal symbols that could follow [nt] in a sentential form
     that is derived from a start symbol. *)
  val follow: Nonterminal.t -> TerminalSet.t

  (**[tfollow t] is the FOLLOW set of the terminal symbol [t]. *)
  val tfollow: Terminal.t -> TerminalSet.t

  (**[sfollow nt] is the symbolic FOLLOW set of the nonterminal symbol [nt].
     It is the set of symbols that could follow [nt] in a sentential form
     that is derived from a start symbol. *)
  val sfollow: Nonterminal.t -> SymbolSet.t

  (**[minimal nt] is the minimal size of a sentence generated by the
     nonterminal symbol [nt]. If this symbol generates an empty language,
     then [minimal nt] is [max_int]. Any productions whose right-hand side
     mentions the [error] token are ignored by this analysis. *)
  val minimal: Nonterminal.t -> int

  (**[minimal'] is analogous to [minimal]. Instead of returning just the
     length of a minimal sentence generated by [nt], it returns a minimal
     sentence. *)
  val minimal': Nonterminal.t -> Terminal.t CompletedNatWitness.t

  (**[minimal_prod prod i] is the minimal size of a sentence generated by
     the production suffix determined by [prod] and [i]. The offset [i] must
     lie in the interval [\[0, n\]], where [n] is the length of production
     [prod]. Any productions whose right-hand side mentions the [error]
     token are ignored by this computation. *)
  val minimal_prod: Production.t -> int -> int

  (**[maximal nt] is the maximal size of a sentence generated by the
     nonterminal symbol [nt]. An unbounded maximal size is represented by
     the special value [max_int]. This analysis is carried out under the
     assumption that every symbol generates a nonempty language. Any
     productions whose right-hand side mentions the [error] token are
     ignored by this analysis. *)
  val maximal: Nonterminal.t -> int

  (**[maximal_prod prod i] is the maximal size of a sentence generated by
     the production suffix determined by [prod] and [i]. The offset [i] must
     lie in the interval [\[0, n\]], where [n] is the length of production
     [prod]. Any productions whose right-hand side mentions the [error]
     token are ignored by this analysis. *)
  val maximal_prod: Production.t -> int -> int

end

(* -------------------------------------------------------------------------- *)

(** {2 [%on_error_reduce] declarations} *)

module OnErrorReduce : sig

  (**[reduce prod] determines whether the left-hand side of the production
     [prod] (a nonterminal symbol) appears in an [%on_error_reduce]
     declaration. *)
  val reduce: Production.t -> bool

  (**[iter] enumerates the nonterminal symbols that appear in an
     [%on_error_reduce] declaration. *)
  val iter: (Nonterminal.t -> unit) -> unit

  (**When two productions can be reduced, in a single state, due to
     [%on_error_reduce] declarations, these productions can be compared,
     using [preferable], to test if one of them takes precedence over the
     other. This is a partial order; two productions may be incomparable. *)
  val preferable: Production.t -> Production.t -> bool

end

(* -------------------------------------------------------------------------- *)

(** {2 Sentences} *)

module Sentence : sig

  (**A sentence is a pair of an optional start symbol and a sequence of
     terminal symbols. Our convention is that the start symbol can be omitted
     if this is unambiguous, that is, if the grammar has exactly one start
     symbol. *)
  type sentence =
    Nonterminal.t option * Terminal.t list

  (**[print style sentence] prints a sentence as a space-separated list of
     terminal symbols. If [style] is [`Abstract] then each terminal symbol is
     printed as a symbolic name. If [style] is [`Concrete] then each terminal
     symbol is printed as its unquoted alias; see [Terminal.unquoted_alias].
     In the latter case, every token must have a token alias: that is,
     [Terminal.every_token_has_an_alias] must be [true]. *)
  val print: [`Abstract | `Concrete] -> sentence -> string

end

(* -------------------------------------------------------------------------- *)

end
