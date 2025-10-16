(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Located
open PlainSyntax

let prod_id producer : identifier =
  value producer.prod_id

let prod_attributes producer =
  producer.prod_attributes

(* -------------------------------------------------------------------------- *)

(* Terminal symbols. *)

(* [tokens grammar] is a list of all (real) tokens in the grammar
   [grammar]. The special tokens "#" and "error" are not included.
   Pseudo-tokens (used in %prec declarations, but never declared
   using %token) are filtered out. *)

let tokens grammar =
  StringMap.fold (fun token properties tokens ->
    if properties.tk_is_declared then token :: tokens else tokens
  ) grammar.tokens []

(* [typed_tokens grammar] is analogous, but includes the OCaml type
   of each token. *)

let typed_tokens grammar =
  StringMap.fold (fun token properties tokens ->
    if properties.tk_is_declared then (token, properties.tk_ocamltype) :: tokens else tokens
  ) grammar.tokens []

(* [ocamltype_of_token grammar symbol] produces the OCaml type
   of the terminal symbol [symbol] in the grammar [grammar].
   This terminal symbol must exist and must not be a pseudo-token.
   [None] is returned if and only if this token does not carry a
   semantic value. *)

let ocamltype_of_token grammar symbol : ocamltype option =
  try
    let properties = StringMap.find symbol grammar.tokens in
    assert properties.tk_is_declared;
    properties.tk_ocamltype
  with Not_found ->
    assert false

(* [alias grammar symbol] returns the token alias of the terminal symbol
   [symbol], if there is one. This terminal symbol must exist and must not
   be a pseudo-token. *)

let alias grammar symbol : alias =
  try
    let properties = StringMap.find symbol grammar.tokens in
    assert properties.tk_is_declared;
    properties.tk_alias
  with Not_found ->
    assert false

let unquoted_alias grammar symbol : alias =
  alias grammar symbol
  |> Option.map MString.unquote

(* -------------------------------------------------------------------------- *)

(* Nonterminal symbols. *)

(* [nonterminals grammar] is a list of all nonterminal symbols in the
   grammar [grammar]. It does not include the artificial start symbols
   [S']. *)

let nonterminals grammar : nonterminal list =
  StringMap.fold (fun nt _ rules -> nt :: rules) grammar.rules []

(* [is_nonterminal grammar symbol] tests whether the symbol [symbol]
   is a nonterminal symbol. It is assumed that this symbol exists,
   that is, it is either a terminal symbol or a nonterminal symbol. *)

let is_nonterminal grammar symbol =
  StringMap.mem symbol grammar.rules

(* [ocamltype_of_symbol grammar symbol] produces the OCaml type
   of the nonterminal symbol [symbol] in the grammar [grammar],
   if it is known. *)

let ocamltype_of_symbol grammar symbol : ocamltype option =
  try
    Some (StringMap.find symbol grammar.types)
  with Not_found ->
    None

(* [ocamltype_of_start_symbol grammar symbol] produces the OCaml type
   of the start symbol [symbol] in the grammar [grammar]. *)

let ocamltype_of_start_symbol grammar symbol : ocamltype =
  try
    StringMap.find symbol grammar.types
  with Not_found ->
    (* Every start symbol should have a type. *)
    assert false

(* -------------------------------------------------------------------------- *)

(* Productions. *)

(* [print_production nt branch] prints the production whose left-hand side
   is the nonterminal symbol [nt] and whose right-hand side is [branch]. *)

let rec print_production nt branch =
  Printf.sprintf "%s ->%s" nt (print_production_rhs branch.producers)

and print_production_rhs producers =
  MString.with_buffer 80 @@ fun b ->
  producers |> List.iter @@ fun producer ->
  Printf.bprintf b " %s" producer.prod_symbol
