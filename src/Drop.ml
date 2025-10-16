(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let accumulate = MList.accumulate
open Attribute
let value = Located.value
(* The source. *)
module S = Syntax
(* The target. *)
module T = PlainSyntax

(* -------------------------------------------------------------------------- *)

(* Most of the translation is straightforward. *)

let drop_parameter (param : S.parameter) : S.symbol =
  (* The grammar should not have any parameterized symbols. *)
  value (Parameter.head param)

let drop_producer ((id, param, attrs) : S.producer) : T.producer =
  { T.prod_id = id;
    T.prod_symbol     = drop_parameter param;
    T.prod_attributes = attrs }

let drop_branch (branch : S.parameterized_branch) : T.branch =
  { T.branch_position  = branch.S.pb_position;
    T.producers        = List.map drop_producer branch.S.pb_producers;
    T.action           = branch.S.pb_action;
    T.prec_annotation  = branch.S.pb_prec_annotation;
    T.production_level = branch.S.pb_production_level;
    T.br_attributes    = branch.S.pb_attributes }

let drop_rule (rule : S.parameterized_rule) : T.rule =
  (* The grammar should not have any parameterized symbols. *)
  assert (rule.S.pr_parameters = []);
  (* The [%public] flag is dropped. *)
  { T.branches = List.map drop_branch rule.S.pr_branches;
    T.positions = rule.S.pr_positions;
    T.inline_flag = rule.S.pr_inline;
    T.attributes = rule.S.pr_attributes }

(* -------------------------------------------------------------------------- *)

(* We must store [%type] declarations and [%on_error_reduce] declarations
   in maps, whereas so far they were represented as lists. *)

let drop_declarations
  (f : 'info1 -> 'info2) (decls : (S.parameter * 'info1) list)
: 'info2 StringMap.t =
  accumulate StringMap.empty decls @@ fun accu (param, info) ->
  let symbol = value (Parameter.head param) in
  StringMap.add symbol (f info) accu

let drop_type_declarations decls =
  drop_declarations value decls

let drop_on_error_reduce_declarations decls =
  drop_declarations (fun x -> x) decls

(* -------------------------------------------------------------------------- *)

(* We must eliminate (that is, desugar) [%attribute] declarations. We examine
   them one by one and attach these attributes with terminal or nonterminal
   symbols, as appropriate. This is entirely straightforward. *)

let add_attribute (g : T.grammar) param attr : T.grammar =
  let symbol = drop_parameter param in
  match StringMap.find symbol g.T.tokens with
  | props ->
      (* This is a terminal symbol. *)
      let props = { props with S.tk_attributes = attr :: props.S.tk_attributes } in
      { g with T.tokens = StringMap.add symbol props g.T.tokens }
  | exception Not_found ->
      match StringMap.find symbol g.T.rules with
      | rule ->
          (* This is a nonterminal symbol. *)
          let rule = { rule with T.attributes = attr :: rule.T.attributes } in
          { g with T.rules = StringMap.add symbol rule g.T.rules }
      | exception Not_found ->
          (* This is an unknown symbol. This should not happen. *)
          assert false

let add_attributes g (params, attrs) =
  accumulate g params @@ fun g param ->
  accumulate g attrs @@ fun g attr ->
  add_attribute g param attr

let add_attributes (decls : (S.parameter list * attributes) list) g =
  accumulate g decls add_attributes

(* -------------------------------------------------------------------------- *)

(* Putting it all together. *)

let drop (g : S.grammar) : T.grammar =
  {
    T.preludes        = g.S.p_preludes;
    T.postludes       = g.S.p_postludes;
    T.parameters      = g.S.p_parameters;
    T.start_symbols   = StringMap.domain g.S.p_start_symbols;
    T.types           = drop_type_declarations g.S.p_types;
    T.tokens          = g.S.p_tokens;
    T.on_error_reduce = drop_on_error_reduce_declarations g.S.p_on_error_reduce;
    T.gr_attributes   = g.S.p_grammar_attributes;
    T.rules           = StringMap.map drop_rule g.S.p_rules
  } |> add_attributes g.S.p_symbol_attributes
