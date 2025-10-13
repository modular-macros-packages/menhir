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
open Located
open Syntax

module Make (Error : sig

  (**[error ranges format ...] reports an error and aborts. *)
  val error : ranges -> ('a, out_channel, unit, 'b) format4 -> 'a

  (**[warning ranges format ...] reports a warning. It does not abort. *)
  val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

end) (X : sig

  (**[ignore_unused_token t] returns [true] if an ignored-token warning
     about the token [t] should be suppressed. *)
  val ignore_unused_token : terminal -> bool

end) = struct

let reserved =
  [ "error" ]

(* -------------------------------------------------------------------------- *)

(* This local module helps share parameters and mutable state. *)

module Run (G : sig val grammar : grammar end) = struct
open G

(* -------------------------------------------------------------------------- *)

(* Check that every start symbol has a definition as a nonterminal symbol
   and has a %type declaration. *)

let is_named nt (param, _ocamltype) =
  Parameter.equal (Parameter.var nt) param

let () =
  grammar.p_start_symbols |> StringMap.iter @@ fun nt pos ->
  if not (StringMap.mem nt grammar.p_rules) then
    Error.error [pos] "the start symbol %s is undefined." nt;
  if not (List.exists (is_named nt) grammar.p_types) then
    Error.error [pos] "the type of the start symbol %s is unspecified." nt

(* -------------------------------------------------------------------------- *)

(* [check_decl kind must_be_nonterminal param] checks that every symbol that
   occurs within the parameter [param] is well-defined. *)

(* The parameter [kind] is the kind of declaration that is being inspected:
   it can be %type, %on_error_reduce, or %attribute. *)

(* The parameter [must_be_nonterminal] determines whether the root symbol
   must be a nonterminal symbol. It is reset to [false] in the recursive
   calls: e.g. in [X(Y,Z)], only [X] must be a nonterminal symbol. *)

let rec check_decl (kind : string) (must_be_nonterminal : bool) param =
  (* Destructure the head and arguments. *)
  let head, params = Parameter.destruct param in
  let head, pos = value head, position head in
  (* Check if [head] is a nonterminal or terminal symbol. *)
  let is_nonterminal =
    StringMap.mem head grammar.p_rules
  and is_terminal =
    StringMap.mem head grammar.p_tokens ||
    List.mem head reserved
  in
  (* If [head] is not satisfactory, fail. *)
  if not (is_terminal || is_nonterminal) then
    Error.error [pos]
      "%s is undefined." head
  ;
  if (must_be_nonterminal && not is_nonterminal) then
    Error.error [pos]
      "%s is a terminal symbol,\n\
       but %s declarations are applicable only to nonterminal symbols."
      (Parameter.print ", " param) kind
  ;
  (* Then, check the children. *)
  List.iter (check_decl kind false) params

(* A variant. *)

let check_decl' kind must_be_nonterminal (param, _) =
    check_decl kind must_be_nonterminal param

(* Special cases. *)

let check_type_decl param =
  check_decl' "%type" true param

let check_on_error_reduce_decl param =
  check_decl' "%on_error_reduce" true param

let check_attribute_decl param =
  check_decl "%attribute" false param

let check_attribute_decls (params, _) =
  List.iter check_attribute_decl params

(* Check that every %type, %on_error_reduce or %attribute declaration
   refers to well-defined symbols and has, at its head, a nonterminal
   symbol. *)

let () =
  List.iter check_type_decl            grammar.p_types;
  List.iter check_on_error_reduce_decl grammar.p_on_error_reduce;
  List.iter check_attribute_decls      grammar.p_symbol_attributes

(* -------------------------------------------------------------------------- *)

(* We record which tokens (terminal symbols) are used. *)

let used =
  ref StringSet.empty

let mark (t : terminal) =
  used := StringSet.add t !used

(* -------------------------------------------------------------------------- *)

(* [check_symbol rule prec symbol] resolves an occurrence of the symbol
   [symbol] and performs several checks about it. *)

(* The parameter [rule] is the rule where this symbol appears. *)

(* The parameter [prec] indicates whether this occurrence of [symbol] is
   a %prec annotation. *)

let check_symbol rule prec (symbol : symbol located) =
  let symbol, pos = value symbol, position symbol in
  if not prec && List.mem symbol rule.pr_parameters then
    (* A parameter of this rule. *)
    ()
  else if not prec && List.mem symbol reserved then
    (* A reserved token. Mark it as used. *)
    mark symbol
  else if not prec && StringMap.mem symbol grammar.p_rules then
    (* A nonterminal symbol. *)
    ()
  else
    match StringMap.find symbol grammar.p_tokens with
    | properties ->
        (* A token or pseudo-token. Mark it as used. *)
        mark symbol;
        if not prec && not properties.tk_is_declared then
          (* A pseudo-token, declared by %left, %right or %nonassoc, cannot
             be used as a normal identifier. It can appear only in a %prec
             annotation. *)
          Error.error [ pos ]
            "The symbol %s has not been declared by %%token,\n\
             so cannot be used here." symbol
    | exception Not_found ->
        (* An unknown symbol. *)
        if prec then
          Error.error [ pos ] "The terminal symbol %s is undefined." symbol
        else
          Error.error [ pos ] "The symbol %s is undefined." symbol

(* -------------------------------------------------------------------------- *)

(* Check that every rule, branch, and producer is well-formed. *)

let check_producer rule (_id, param, _ : producer) =
  Parameter.iter (check_symbol rule false) param

let check_branch rule branch =
  let producers = branch.pb_producers in
  (* Check each producer. *)
  producers |> List.iter (check_producer rule);
  (* Check the branch's optional %prec annotation. *)
  branch.pb_prec_annotation |> Option.iter (check_symbol rule true);
  (* Check that no variable is bound twice, i.e.,
     no two producers define the same variable. *)
  let cmp (x1, _, _) (x2, _, _) = String.compare (value x1) (value x2) in
  MList.foreach_duplicate cmp producers @@ fun (x, _, _) ->
  Error.error [branch.pb_position]
    "there are multiple producers named %s in this sequence."
    (value x)

let check_rule_has_distinct_formal_parameters rule =
  MList.foreach_duplicate String.compare rule.pr_parameters @@ fun x ->
    Error.error rule.pr_positions
      "several parameters of this rule are named \"%s\"." x

let check_inline_rule_has_no_attribute rule =
  match rule.pr_inline, rule.pr_attributes with
  | true, attr :: _ ->
      Error.error [attr.origin]
        "the nonterminal symbol %s is declared %%inline.\n\
         It cannot carry an attribute."
        rule.pr_nt
  | true, []
  | false, _ ->
      ()

let check_rule nt rule =
  (* The fact that the formal parameters of each rule have distinct names
     has already been checked in JoinGrammars. Nevertheless, it costs us
     only a few lines of code to repeat this check here. *)
  check_rule_has_distinct_formal_parameters rule;
  (* Check each branch. *)
  List.iter (check_branch rule) rule.pr_branches;
  (* It is forbidden to use %inline on a %start symbol. *)
  if (rule.pr_inline && StringMap.mem nt grammar.p_start_symbols) then
     Error.error rule.pr_positions
       "%s cannot be both a start symbol and inlined." nt;
  (* If a rule is marked %inline, then it must not carry an attribute. *)
  check_inline_rule_has_no_attribute rule

(* Check every rule. *)

let () =
  grammar.p_rules |> StringMap.iter check_rule

(* -------------------------------------------------------------------------- *)

(* Emit a warning about each unused token. *)

(* These warnings can be disabled by the user-supplied function
   [X.ignore_unused_token]. *)

let () =
  grammar.p_tokens |> StringMap.iter @@ fun t properties ->
  if not (X.ignore_unused_token t || StringSet.mem t !used) then
    Error.warning [properties.tk_position]
      "the token %s is unused." t

(* -------------------------------------------------------------------------- *)

let finished =
  ()

end (* Run *)

let check grammar =
  let module R = Run(struct let grammar = grammar end) in
  R.finished

end (* Make *)
