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
open Syntax
open Located

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

(* If a rule is marked %inline, then it must not carry an attribute. *)
let check_inline_attribute prule =
  match prule.pr_inline, prule.pr_attributes with
  | true, attr :: _ ->
      Error.error
        [attr.origin]
        "the nonterminal symbol %s is declared %%inline.\n\
         It cannot carry an attribute."
        prule.pr_nt
  | true, []
  | false, _ ->
      ()

let reserved =
  [ "error" ]

let check_identifier_reference
  mark_token_as_used grammar prule is_prec
  (s : symbol located) =
  let s = value s
  and p = position s in
  if not is_prec && List.mem s prule.pr_parameters then
    (* A parameter of this rule. *)
    ()
  else if not is_prec && List.mem s reserved then
    (* A reserved token. *)
    mark_token_as_used s
  else if not is_prec && StringMap.mem s grammar.p_rules then
    (* A nonterminal symbol. *)
    ()
  else match StringMap.find s grammar.p_tokens with
    | prop ->
        (* A token or pseudo-token. Mark it as used. *)
        mark_token_as_used s;
        if not is_prec && not prop.tk_is_declared then
          (* A pseudo-token, declared by %left, %right or %nonassoc,
             cannot be used as a normal identifier. It can be only in
             a %prec annotation. *)
          Error.error [ p ]
            "The symbol %s has not been declared by %%token,\n\
             so cannot be used here." s
    | exception Not_found ->
        (* An unknown symbol. *)
        if is_prec then
          Error.error [ p ] "The terminal symbol %s is undefined." s
        else
          Error.error [ p ] "The symbol %s is undefined." s

let check_parameterized_grammar_is_well_defined grammar =

  (* Every start symbol is defined and has a %type declaration. *)
  StringMap.iter
    (fun nonterminal p ->
       if not (StringMap.mem nonterminal grammar.p_rules) then
         Error.error [p] "the start symbol %s is undefined." nonterminal;
       if not (List.exists (function
                            | ParamVar x, _ -> value x = nonterminal
                            | _ -> false) grammar.p_types) then
         Error.error [p]
           "the type of the start symbol %s is unspecified." nonterminal;
    ) grammar.p_start_symbols;

  (* Every %type definition refers to well-defined (terminal or nonterminal)
     symbols and has, at its head, a nonterminal symbol. *)
  (* Same check for %on_error_reduce definitions. *)

  let rec check (kind : string) (must_be_nonterminal : bool) (p : Syntax.parameter) =
    (* Destructure head and arguments. *)
    let head, ps = Parameter.destruct p in
    let head = value head
    and pos = position head in
    (* Check if [head] is a nonterminal or terminal symbol. *)
    let is_nonterminal = StringMap.mem head grammar.p_rules
    and is_terminal = StringMap.mem head grammar.p_tokens || List.mem head reserved in
    (* If [head] is not satisfactory, error. *)
    if not (is_terminal || is_nonterminal) then
      Error.error [pos]
             "%s is undefined." head;
    if (must_be_nonterminal && not is_nonterminal) then
      Error.error [pos]
             "%s is a terminal symbol,\n\
              but %s declarations are applicable only to nonterminal symbols."
             (Parameter.print ", " p) kind;
    (* Then, check the arguments. *)
    List.iter (check kind false) ps
  in

  let check_fst kind must_be_nonterminal (p, _) =
    check kind must_be_nonterminal p
  in

  List.iter (check_fst "%type" true) grammar.p_types;
  List.iter (check_fst "%on_error_reduce" true) grammar.p_on_error_reduce;
  List.iter (fun (params, _) ->
    List.iter (check "%attribute" false) params
  ) grammar.p_symbol_attributes;

  (* Every reference to a symbol is well defined. *)
  let used_tokens = ref StringSet.empty in
  let mark_token_as_used token =
    used_tokens := StringSet.add token !used_tokens
  in
    StringMap.iter
      (fun k prule ->

         let check_identifier_reference =
           check_identifier_reference
             mark_token_as_used grammar prule
         in

         (* The formal parameters of each rule must have distinct names. *)
         (* This has already been checked in JoinGrammars. *)

         (* Check each branch. *)
         List.iter (fun branch ->
           let producers = branch.pb_producers in
           ignore (List.fold_left

            (* Check the producers. *)
            (fun already_seen (id, p, _) ->
               let already_seen =
                 (* Check the producer id is unique. *)
                 if StringSet.mem (value id) already_seen then
                   Error.error [position id]
                     "there are multiple producers named %s in this sequence."
                     (value id);
                 StringSet.add (value id) already_seen
               in
               (* Check that the producer is defined somewhere. *)
               Parameter.iter (check_identifier_reference false) p;
               already_seen

            ) StringSet.empty producers);

           Option.iter (check_identifier_reference true) branch.pb_prec_annotation)

         prule.pr_branches;

         (* It is forbidden to use %inline on a %start symbol. *)
         if (prule.pr_inline
             && StringMap.mem k grammar.p_start_symbols) then
           Error.error prule.pr_positions
                "%s cannot be both a start symbol and inlined." k;

         (* If a rule is marked %inline, then it must not carry an attribute. *)
         check_inline_attribute prule

      ) grammar.p_rules;

  (* Check that every token is used. *)
      StringMap.iter (fun t properties ->
        if not (X.ignore_unused_token t || StringSet.mem t !used_tokens) then
          Error.warning [properties.tk_position]
            "the token %s is unused." t
      ) grammar.p_tokens

let check =
  check_parameterized_grammar_is_well_defined

end (* Make *)
