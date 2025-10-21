(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module constructs a view of the grammar that is well-suited to the
   computations that the middle-end needs to perform. In particular, it
   offers abstract types of terminal symbols, nonterminal symbols, symbols,
   and productions, as well as efficient sets and maps. Furthermore, it
   performs several analyses of the grammar, including NONEMPTY, NULLABLE,
   FIRST, FOLLOW, MINIMAL, MAXIMAL. *)

open Attribute
open PlainSyntax

(**The functor [Make] transforms a grammar in [PlainSyntax] format
   into a rich internal representation of the grammar. *)
module Make (G : sig
  val grammar: grammar
end) () : sig

type ocamltype = OCamlType.ocamltype =
  | Declared of string located
  | Inferred of string

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
module TerminalMap : API.MAP with type key = Terminal.t

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

  (**[domain m] is the domain of the map [m], that is, the list of keys
     for which an entry exists in the map [m]. *)
  val domain: 'a t -> key list

  (**[init f xs] creates a map whose keys are the elements [x] found in
     the list [xs] and the datum associated with [x] is [f x]. *)
  val init: (key -> 'a) -> key list -> 'a t

end

(* -------------------------------------------------------------------------- *)

(** {2 Productions} *)

module Production : sig

  (* This is the type of productions. This includes user-defined
     productions as well as the internally generated productions
     associated with the start symbols. *)

  type index

  (* Comparison. *)

  val compare: index -> index -> int

  (* Productions can be converted to integers and back. This is unsafe
     and should be avoided as much as possible. This feature is
     exploited, for efficiency, in the encoding of items. *)

  val p2i: index -> int
  val i2p: int -> index

  (* The number of productions. *)

  val n: int

  (* These map a production index to the production's definition, that
     is, a nonterminal (the left-hand side) and an array of symbols
     (the right-hand side). *)

  val def: index -> Nonterminal.t * Symbol.t array
  val nt: index -> Nonterminal.t
  val rhs: index -> Symbol.t array
  val length: index -> int

  (* This maps a production index to an array of the identifiers that
     should be used for naming the semantic values of the symbols in
     the right-hand side. *)

  val identifiers: index -> Syntax.identifier array

  (* This maps a production index to the production's semantic action.
     This function is not applicable to a start production. *)

  val action: index -> Syntax.action

  (* [positions prod] is a list of the positions associated with
     production [prod]. This is usually a singleton list, but there
     can be more than one position for start productions when the
     definition of the corresponding start symbol is split over
     multiple files. *)

  val positions: index -> ranges

  (* [rhs_attributes prod] returns an array of the attributes attached with
     each element in the right-hand side of the production [prod]. *)

  val rhs_attributes: index -> attributes array

  (* [prec_decl prod] returns the optional %prec annotation carried by the
     production [prod]. This information plays a role in the resolution of
     shift/reduce conflicts. *)
  val prec_decl : index -> symbol located option

  (* [production_level prod] returns the production level of the production
     [prod]. This information plays a role in the resolution of reduce/reduce
     conflicts. *)

  val production_level : index -> production_level

  (* [attributes prod] is the list of attributes attached with the
     production [prod]. *)

  val attributes: index -> attributes

  (* Creation of a table indexed by productions. *)

  val init: (index -> 'a) -> 'a array

  (* Iteration over all productions. The order in which elements
     are examined, and the order of [map]'s output list, correspond
     to the numeric indices produced by [p2i] above. *)

  val iter: (index -> unit) -> unit
  val fold: (index -> 'a -> 'a) -> 'a -> 'a
  val map: (index -> 'a) -> 'a list
  val amap: (index -> 'a) -> 'a array

  (* Iteration over all productions, except the start productions. *)

  val iterx: (index -> unit) -> unit
  val foldx: (index -> 'a -> 'a) -> 'a -> 'a
  val mapx: (index -> 'a) -> 'a list

  (* This maps a (user) non-terminal start symbol to the corresponding
     start production. *)

  val startsymbol2startprod: Nonterminal.t -> index

  (* Iteration over the productions associated with a specific
     nonterminal. *)

  val iternt: Nonterminal.t -> (index -> unit) -> unit
  val foldnt: Nonterminal.t -> (index -> 'a -> 'a) -> 'a -> 'a
  val mapnt:  Nonterminal.t -> (index -> 'a) -> 'a list

  (* This allows determining whether a production is a start
     production. If it is a start production, the start symbol that it
     is associated with is returned. If it is a regular production,
     nothing is returned. *)

  val classify: index -> Nonterminal.t option

  (* [is_start] is easier to use than [classify] when the start symbol
     is not needed. *)

  val is_start: index -> bool

  (* The integer [start] is published so as to allow the table back-end
     to produce code for [is_start]. It should not be used otherwise. *)

  val start: int

  (* [error_free prod] returns [true] if the right-hand side of the
     production [prod] does *not* contain the [error] token. *)

  val error_free: index -> bool

  (* This produces a string representation of a production. It should
     never be applied to a start production, as we do not wish users
     to become aware of the existence of these extra productions. *)

  val print: index -> string

  (* This produces a representation of a production as a string of the form
     "accepting <nt>" or "reducing <nt> -> <rhs>". The Boolean flag [gerund]
     allows choosing between gerund and infinitive forms. *)

  val describe: bool -> index -> string

  (* Tabulation of a function over productions. *)

  val tabulate: (index -> 'a) -> (index -> 'a)

  (* Sum of an integer function over productions. *)

  val sum: (index -> int) -> int

end

(* -------------------------------------------------------------------------- *)

(* {2 Maps of Productions} *)

module ProductionMap : sig

  include API.MAP with type key = Production.index

  (* Iteration over the start productions only. *)

  val start: (Production.index -> 'a) -> 'a t

end

(* -------------------------------------------------------------------------- *)
(* This flag tells whether the [error] token appears in at least one
   production. *)

val grammar_uses_error_token: bool

(* -------------------------------------------------------------------------- *)

(* {Analyses of the Grammar} *)

module Analysis : sig

  (* [nonempty nt] is true if and only if the nonterminal symbol [nt]
     generates an nonempty language. *)

  val nonempty: Nonterminal.t -> bool

  (* [nullable nt] is the NULLABLE flag of the non-terminal symbol [nt].
     That is, it is true if and only if this symbol produces the empty
     word [epsilon]. *)

  val nullable: Nonterminal.t -> bool
  val nullable_symbol: Symbol.t -> bool

  (* [first nt] is the FIRST set of the non-terminal symbol [nt]. *)

  val first: Nonterminal.t -> TerminalSet.t
  val first_symbol: Symbol.t -> TerminalSet.t

  (* [nullable_first_prod prod i] considers the suffix of the production
     [prod] defined by offset [i]. It returns its NULLABLE flag as well
     as its FIRST set. The offset [i] must be contained between [0] and
     [n], inclusive, where [n] is the length of production [prod]. *)

  val nullable_first_prod: Production.index -> int -> bool * TerminalSet.t

  (* [first_prod_lookahead prod i t] computes [FIRST(alpha.t)], where [alpha]
     is the suffix of the production defined by offset [i], and [t] is a
     terminal symbol. The offset [i] must be contained between [0] and [n],
     inclusive, where [n] is the length of production [prod]. *)

  val first_prod_lookahead: Production.index -> int -> Terminal.t -> TerminalSet.t

  (* [explain_first_rhs tok rhs i] explains why the token [tok] appears
     in the FIRST set for the string of symbols found at offset [i] in
     the array [rhs]. *)

  val explain_first_rhs: Terminal.t -> Symbol.t array -> int -> string

  (* [follow nt] is the FOLLOW set of the non-terminal symbol [nt], that
     is, the set of terminal symbols that could follow an expansion of
     [nt] in a valid sentence. *)

  val follow: Nonterminal.t -> TerminalSet.t
  val tfollow: Terminal.t -> TerminalSet.t
  val sfollow: Nonterminal.t -> SymbolSet.t

  (* [attributes] are the attributes attached with the grammar. *)

  val attributes: attributes

  (**[minimal nt] is the minimal size of a sentence generated by the
     nonterminal symbol [nt]. If this symbol generates an empty language,
     then [minimal nt] is [max_int]. Any productions whose right-hand side
     mentions the [error] token are ignored in this computation. *)
  val minimal: Nonterminal.t -> int
  val minimal': Nonterminal.t -> Terminal.t CompletedNatWitness.t

  (**[minimal_prod prod i] is the minimal size of a sentence generated by
     the suffix of the production [prod] defined by the offset [i]. Any
     productions whose right-hand side mentions the [error] token are
     ignored in this computation. *)
  val minimal_prod: Production.index -> int -> int

  (**[maximal nt] is the maximal size of a sentence generated by the
     nonterminal symbol [nt]. An unbounded maximal size is represented by
     the special value [max_int]. This analysis is carried out under the
     assumption that every symbol generates a nonempty language. Any
     productions whose right-hand side mentions the [error] token are
     ignored in this computation. *)
  val maximal: Nonterminal.t -> int
  val maximal': Nonterminal.t -> NatInfinityMax.t

  (**[maximal_prod prod i] is the maximal size of a sentence generated by
     the suffix of the production [prod] defined by the offset [i]. Any
     productions whose right-hand side mentions the [error] token are
     ignored in this computation. *)
  val maximal_prod: Production.index -> int -> int

end

(* -------------------------------------------------------------------------- *)
(* [%on_error_reduce] declarations. *)

module OnErrorReduce : sig

  (* [reduce prod] tells whether the left-hand side of [prod] (a nonterminal
     symbol) appears in an [%on_error_reduce] declaration. *)

  val reduce: Production.index -> bool

  (* [iter f] applies the function [f] in turn, in an arbitrary order, to
     every nonterminal symbol that appears in an [%on_error_reduce]
     declaration. *)

  val iter: (Nonterminal.t -> unit) -> unit

  (* When two productions could be reduced, in a single state, due to
     [%on_error_reduce] declarations, these productions can be compared, using
     [preferable], to test if one of them takes precedence over the other.
     This is a partial order; two productions may be incomparable. *)

  val preferable: Production.index -> Production.index -> bool

end

(* -------------------------------------------------------------------------- *)
(* Sentences. *)

module Sentence : sig

  (**A sentence is a pair of an optional start symbol and a sequence of
     terminal symbols. Our convention is the start symbol can be omitted if
     this is unambiguous, that is, if the grammar has exactly one start
     symbol. *)
  type sentence =
    Nonterminal.t option * Terminal.t list

  (**[print style sentence] prints a sentence as a space-separated list of
     symbolic token names. The [style] parameter indicates whether the
     sentence should be displayed in concrete syntax; if it is [`Concrete],
     then every token must have a token alias, that is,
     [Terminal.every_token_has_an_alias] must be [true]. *)
  val print: [`Abstract | `Concrete] -> sentence -> string

end

(* -------------------------------------------------------------------------- *)

end (* module Make *)
