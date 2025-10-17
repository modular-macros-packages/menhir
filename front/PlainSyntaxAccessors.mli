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

(**This module offers a number of accessors that help exploit a grammar. *)

(**[prod_id] returns the identifier of a producer, that is, the name of
   the variable that is bound to the semantic value of this producer. *)
val prod_id : producer -> identifier

(**[prod_attributes] returns the attributes of a producer. *)
val prod_attributes : producer -> attributes

(**[ocamltype_of_token grammar symbol] produces the OCaml type of the terminal
   symbol [symbol] in the grammar [grammar]. This terminal symbol must exist
   and must not be a pseudo-token. [None] is returned if and only if this
   token does not carry a semantic value. *)
val ocamltype_of_token : grammar -> symbol -> ocamltype option

(**[ocamltype_of_symbol grammar symbol] produces the OCaml type of the
   nonterminal symbol [symbol] in the grammar [grammar], if this type is
   known. [None] is returned if this type is unknown. *)
val ocamltype_of_symbol : grammar -> symbol -> ocamltype option

(**[ocamltype_of_start_symbol grammar symbol] produces the OCaml type of the
   start symbol [symbol] in the grammar [grammar]. The type of a start symbol
   is always known, as it must be declared. *)
val ocamltype_of_start_symbol : grammar -> symbol -> ocamltype

(**[tokens grammar] is a list of all (real) tokens in the grammar [grammar].
   The special tokens "#" and "error" are not included. Pseudo-tokens (used
   in %prec declarations, but never declared using %token) are filtered out. *)
val tokens : grammar -> terminal list

(**[typed_tokens grammar] is analogous to [tokens grammar], but includes
   the OCaml type of each token, if it is known. *)
val typed_tokens : grammar -> (terminal * ocamltype option) list

(**[nonterminals grammar] is a list of all nonterminal symbols in the grammar
   [grammar]. It does not include the artificial start symbols [S'] that
   Menhir creates internally. *)
val nonterminals : grammar -> nonterminal list

(**[is_nonterminal grammar symbol] tests whether the symbol [symbol] is a
   nonterminal symbol. This symbol must exist; that is, it must be a terminal
   symbol or a nonterminal symbol. *)
val is_nonterminal : grammar -> symbol -> bool

(**[unquoted_alias grammar symbol] returns the token alias of the terminal
   symbol [symbol], if there is one, deprived of its surrounding double quotes
   and unescaped; see [MString.unquote]. The terminal symbol [symbol] must
   exist and must not be a pseudo-token. *)
val unquoted_alias : grammar -> symbol -> alias

(**[print_production nt branch] prints the production whose left-hand side
   is the nonterminal symbol [nt] and whose right-hand side is [branch].
   An example of the result is [A -> a A B]. *)
val print_production : nonterminal -> branch -> string
