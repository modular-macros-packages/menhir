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

let prod_id producer =
  value producer.prod_id

let prod_attributes producer =
  producer.prod_attributes

let ocamltype_of_token grammar symbol =
  match StringMap.find symbol grammar.tokens with
  | properties ->
      assert properties.tk_is_declared;
      properties.tk_ocamltype
  | exception Not_found ->
      assert false

let ocamltype_of_symbol grammar symbol =
  try
    Some (StringMap.find symbol grammar.types)
  with Not_found ->
    None

let ocamltype_of_start_symbol grammar symbol =
  try
    StringMap.find symbol grammar.types
  with Not_found ->
    assert false

let tokens grammar =
  StringMap.fold (fun token properties tokens ->
    if properties.tk_is_declared
    then token :: tokens
    else tokens
  ) grammar.tokens []

let typed_tokens grammar =
  StringMap.fold (fun token properties tokens ->
    if properties.tk_is_declared
    then (token, properties.tk_ocamltype) :: tokens
    else tokens
  ) grammar.tokens []

let nonterminals grammar =
  StringMap.fold (fun nt _ rules -> nt :: rules) grammar.rules []

let is_nonterminal grammar symbol =
  let result = StringMap.mem symbol grammar.rules in
  assert (result || StringMap.mem symbol grammar.tokens || symbol = "error");
  result

let alias grammar symbol =
  try
    let properties = StringMap.find symbol grammar.tokens in
    assert properties.tk_is_declared;
    properties.tk_alias
  with Not_found ->
    assert false

let unquoted_alias grammar symbol =
  alias grammar symbol
  |> Option.map MString.unquote

let rec print_production nt branch =
  Printf.sprintf "%s ->%s" nt (print_production_rhs branch.producers)

and print_production_rhs producers =
  MString.with_buffer 80 @@ fun b ->
  producers |> List.iter @@ fun producer ->
  Printf.bprintf b " %s" producer.prod_symbol
