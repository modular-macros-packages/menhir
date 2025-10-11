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
open Syntax

module Make (Error : sig

  (**[error ranges format ...] reports an error and aborts. *)
  val error : ranges -> ('a, out_channel, unit, 'b) format4 -> 'a

end) = struct

(* -------------------------------------------------------------------------- *)

(* [update_properties] updates an existing token-property record with
   the information found in a %token declaration at position [pos].
   This information includes [ocamltype], [t], [alias], and [attrs]. *)

let update_properties properties pos ocamltype t alias attrs =
  (* If the previous definition was a %token declaration (as opposed to
     a %left, %right, or %nonassoc specification), signal an error. *)
  if properties.tk_is_declared then
    Error.error [pos] "the token %s has multiple definitions." t;
  (* Otherwise, update the previous definition. *)
  { properties with
    tk_is_declared = true;
    tk_position    = pos;
    tk_ocamltype   = ocamltype;
    tk_alias       = alias;
    tk_attributes  = attrs }

(* [create_properties] creates a new token-property record based on the
   information found in a %token declaration. *)

let create_properties pos ocamltype t alias attrs =
  ignore t;
  { tk_associativity = UndefinedAssoc;
    tk_precedence    = UndefinedPrecedence;
    tk_is_declared   = true;
    tk_position      = pos;
    tk_ocamltype     = ocamltype;
    tk_alias         = alias;
    tk_attributes    = attrs }

(* [implicit_properties] creates a new token-property record that contains
   no information except a position. The token is considered not declared.
   This is used when a %left, %right or %nonassoc declaration is encountered
   and the token that is named in it has never been encountered before. *)

let implicit_properties pos =
  { tk_associativity = UndefinedAssoc;
    tk_precedence    = UndefinedPrecedence;
    tk_is_declared   = false;
    tk_position      = pos;
    tk_ocamltype     = None;
    tk_alias         = None;
    tk_attributes    = [] }

(* -------------------------------------------------------------------------- *)

(* [join_declaration grammar decl] adds the declaration [decl] to the
   grammar [grammar]. *)

let join_declaration (grammar : grammar) decl : grammar =
  let pos = position decl in
  match value decl with

  (* We store preludes (code fragments) in the joined grammar in an
     arbitrary order, yet we preserve the order of two preludes that
     originate in the same source file is preserved. *)

  | DCode code ->
      { grammar with p_preludes = grammar.p_preludes @ [ code ] }

  (* Functor parameters are treated in the same way. *)

  | DParameter fragment ->
      { grammar with p_parameters = grammar.p_parameters @ [ fragment ] }

  (* Token declarations are recorded. Things are made somewhat difficult by
     the fact that %token and %left-%right-%nonassoc declarations are
     independent. *)

  (* If a token carries an alias, it is recorded in the field [tk_alias]. *)

  | DToken (ocamltype, t, alias, attrs) ->
      (* Create or update a token-property record for the token [t]. *)
      let properties =
        match StringMap.find t grammar.p_tokens with
        | exception Not_found ->
            create_properties pos ocamltype t alias attrs
        | properties ->
            update_properties properties pos ocamltype t alias attrs
      in
      (* Store this token-property record. *)
      { grammar with
        p_tokens = StringMap.add t properties grammar.p_tokens }

  (* Start symbols. *)

  | DStart nt ->
      { grammar with
        p_start_symbols = StringMap.add nt pos grammar.p_start_symbols }

  (* Type declarations for nonterminals. *)

  | DType (ocamltype, nt) ->
      { grammar with
        p_types = (nt, locate pos ocamltype) :: grammar.p_types }

  (* Reductions on error for nonterminals. *)

  | DOnErrorReduce (nt, prec) ->
      { grammar with
        p_on_error_reduce = (nt, prec) :: grammar.p_on_error_reduce }

  (* Token associativity and precedence. *)

  | DTokenProperties (t, assoc, prec) ->
      (* Retrieve the property record for this token, creating one if none
         existed (the token is then deemed not declared). *)
      let properties =
        try  StringMap.find t grammar.p_tokens
        with Not_found -> implicit_properties pos
      in
      (* Reject duplicate precedence declarations. *)
      if properties.tk_associativity <> UndefinedAssoc then
        Error.error [ pos; properties.tk_position ]
          "there are multiple precedence declarations for token %s." t;
      (* Record the new declaration. *)
      let tk_associativity = assoc and tk_precedence = prec in
      let properties = { properties with tk_associativity; tk_precedence } in
      { grammar with p_tokens = StringMap.add t properties grammar.p_tokens }

  | DGrammarAttribute attr ->
      { grammar with
        p_grammar_attributes = attr :: grammar.p_grammar_attributes }

  | DSymbolAttributes (actuals, attrs) ->
      { grammar with
        p_symbol_attributes = (actuals, attrs) :: grammar.p_symbol_attributes }

(* [join_declarations decls grammar] adds the declarations [decls] to the
   grammar [grammar]. *)

let join_declarations decls grammar =
  List.fold_left join_declaration grammar decls

(* -------------------------------------------------------------------------- *)

(* [join_postlude] adds an optional postlude to a grammar.
   Postludes are stored in an arbitrary order. *)

let join_postlude postlude grammar =
  match postlude with
  | None ->
      grammar
  | Some postlude ->
      { grammar with p_postludes = postlude :: grammar.p_postludes }

(* -------------------------------------------------------------------------- *)

(* We rewrite definitions when nonterminals are renamed. The
   renaming [phi] is an association list of names to names. *)

type renaming =
   (nonterminal * nonterminal) list

let identity_renaming =
  []

let rewrite_nonterminal (phi : renaming) nt =
  try
    List.assoc nt phi
  with Not_found ->
    nt

let rewrite_parameter phi parameter =
  Parameter.map (Located.map (rewrite_nonterminal phi)) parameter

let rewrite_producer phi ((ido, parameter, attrs) : producer) =
  ido, rewrite_parameter phi parameter, attrs

let rewrite_branch phi branch =
  { branch with pb_producers = List.map (rewrite_producer phi) branch.pb_producers }

let rewrite_branches phi branches =
  match phi with
  | [] ->
      branches
  | _ ->
      List.map (rewrite_branch phi) branches

let fresh_counter = ref 0

let names = ref StringSet.empty

let use_name name =
  names := StringSet.add name !names

let used_name name =
  StringSet.mem name !names

let rec fresh ?(hint = "v") () =
  let name =
    incr fresh_counter;
    hint ^ string_of_int !fresh_counter
  in
    if used_name name then
      fresh ~hint ()
    else (
      use_name name;
      name
    )

(* Alpha conversion of [prule]. We rename bound parameters using
   fresh names. *)
let alphaconvert_rule parameters prule =
  let phi =
    List.combine parameters (List.map (fun x -> fresh ~hint:x ()) parameters)
  in
    { prule with
        pr_parameters  = List.map (rewrite_nonterminal phi) prule.pr_parameters;
        pr_branches    = rewrite_branches phi prule.pr_branches
    }

(* Rewrite a rule taking bound names into account. We rename parameters
   to avoid capture. *)
let rewrite_rule phi prule =
  let ids =
    List.fold_left (fun acu (f, d) -> StringSet.add f (StringSet.add d acu))
      StringSet.empty phi
  in
  let captured_parameters =
    List.filter (fun p -> StringSet.mem p ids) prule.pr_parameters
  in
  let prule =
    alphaconvert_rule captured_parameters prule
  in
    { prule with
        pr_nt = rewrite_nonterminal phi prule.pr_nt;
        pr_branches = rewrite_branches phi prule.pr_branches }

let rewrite_rules phi rules =
  List.map (rewrite_rule phi) rules

let rewrite_grammar phi grammar =
  (* We assume that [phi] affects only private symbols, so it does
     not affect the start symbols. *)
  if phi = identity_renaming then
    grammar
  else
    { grammar with pg_rules = rewrite_rules phi grammar.pg_rules }

(* -------------------------------------------------------------------------- *)
(* To rename (internalize) a nonterminal, we prefix it with its filename.
   This guarantees that names are unique. *)

(* [valid c] determines whether the character [c] can appear in the name
   of a nonterminal symbol. *)

let valid c =
  match c with
  | 'A' .. 'Z'
  | 'a' .. 'z'
  | '_'
  | '\192' .. '\214'
  | '\216' .. '\246'
  | '\248' .. '\255'
  | '0' .. '9' ->
      true
  | _ ->
      false

let restrict filename =
  filename
  |> Filename.remove_extension
  |> String.map (fun c -> if valid c then c else '_')

let rename nt filename =
  let name = restrict filename ^ "_" ^ nt in
  if used_name name then
    fresh ~hint:name ()
  else
    (use_name name; name)

(* -------------------------------------------------------------------------- *)
type symbol_kind =

  (* The nonterminal is declared public at a particular position. *)
  | PublicNonTerminal of Range.range

  (* The nonterminal is declared (nonpublic) at a particular position. *)
  | PrivateNonTerminal of Range.range

  (* The symbol is a token. *)
  | Token of properties

  (* We do not know yet what the symbol means.
     This is defined in the sequel or it is free in the partial grammar. *)
  | DontKnow of Range.range

type symbol_table =
    (symbol, symbol_kind) Hashtbl.t

let find_symbol (symbols : symbol_table) symbol =
  Hashtbl.find symbols symbol

let add_in_symbol_table (symbols : symbol_table) symbol kind =
  use_name symbol;
  Hashtbl.add symbols symbol kind;
  symbols

let replace_in_symbol_table (symbols : symbol_table) symbol kind =
  Hashtbl.replace symbols symbol kind;
  symbols

let empty_symbol_table () : symbol_table =
  Hashtbl.create 13

let store_symbol (symbols : symbol_table) symbol kind =
  match find_symbol symbols symbol, kind with

  (* The symbol is not known so far. Add it. *)
  | exception Not_found ->
      add_in_symbol_table symbols symbol kind

  (* There are two definitions of this symbol in one grammatical unit
     (that is, one .mly file), and at least one of them is private.
     This is forbidden. *)
  | PrivateNonTerminal p, PrivateNonTerminal p'
  | PublicNonTerminal p, PrivateNonTerminal p'
  | PrivateNonTerminal p, PublicNonTerminal p' ->
      Error.error [ p; p']
        "the nonterminal symbol %s is multiply defined.\n\
         Only %%public symbols can have split definitions."
        symbol

  (* The symbol is known to be a token but declared as a nonterminal.*)
  | Token tkp, (PrivateNonTerminal p | PublicNonTerminal p)
  | (PrivateNonTerminal p | PublicNonTerminal p), Token tkp ->
      Error.error [ p; tkp.tk_position ]
           "the identifier %s is a reference to a token."
           symbol

  (* In the following cases, we do not gain any piece of information.
     As of 2017/03/29, splitting the definition of a %public nonterminal
     symbol is permitted. (It used to be permitted over multiple units,
     but forbidden within a single unit.) *)
  | _, DontKnow _
  | Token _, Token _
  | PublicNonTerminal _, PublicNonTerminal _ ->
      symbols

  (* We learn that the symbol is a nonterminal or a token. *)
  | DontKnow _, _ ->
      replace_in_symbol_table symbols symbol kind

let store_used_symbol position tokens symbols symbol =
  let kind =
    try
      Token (StringMap.find symbol tokens)
    with Not_found ->
      DontKnow position
  in
  store_symbol symbols symbol kind

let non_terminal_is_not_reserved symbol positions =
  if symbol = "error" then
    Error.error positions
      "%s is reserved and thus cannot be used \
       as a non-terminal symbol." symbol

let non_terminal_is_not_a_token tokens symbol positions =
  try
    let tkp = StringMap.find symbol tokens in
      Error.error (positions @ [ tkp.tk_position ])
         "the identifier %s is a reference to a token."
         symbol
  with Not_found -> ()

let store_public_nonterminal tokens symbols symbol positions =
  non_terminal_is_not_reserved symbol positions;
  non_terminal_is_not_a_token tokens symbol positions;
  store_symbol symbols symbol (PublicNonTerminal (List.hd positions))

let store_private_nonterminal tokens symbols symbol positions =
  non_terminal_is_not_reserved symbol positions;
  non_terminal_is_not_a_token tokens symbol positions;
  store_symbol symbols symbol (PrivateNonTerminal (List.hd positions))

let is_private_symbol t x =
  try
    match Hashtbl.find t x with
      | PrivateNonTerminal _ ->
          true
      | _ ->
          false
  with Not_found ->
    false

let fold_on_private_symbols f init t =
  Hashtbl.fold
    (fun k -> function PrivateNonTerminal _ -> (fun acu -> f acu k)
       | _ -> (fun acu -> acu))
    t init

let fold_on_public_symbols f init t =
  Hashtbl.fold
    (fun k -> function PublicNonTerminal _ -> (fun acu -> f acu k)
       | _ -> (fun acu -> acu))
    t init

let iter_on_only_used_symbols f t =
  Hashtbl.iter
    (fun k -> function DontKnow pos -> f k pos
       | _ -> ())
    t

let symbols_of grammar (pg : Syntax.partial_grammar) =
  let tokens = grammar.p_tokens in
  let symbols_of_rule symbols prule =
    let rec store_except_rule_parameters symbols parameter =
      let symbol, parameters = Parameter.destruct parameter in
      (* Process the reference to [symbol]. *)
      let symbols =
        if List.mem (value symbol) prule.pr_parameters then
          (* Rule parameters are bound locally, so they are not taken into account. *)
          symbols
        else
          store_used_symbol (position symbol) tokens symbols (value symbol)
      in
      (* Process the parameters. *)
      List.fold_left store_except_rule_parameters symbols parameters
    in

    (* Analyse each branch. *)
    let symbols = List.fold_left (fun symbols branch ->
      List.fold_left (fun symbols (_, p, _) ->
        store_except_rule_parameters symbols p
      ) symbols branch.pb_producers
    ) symbols prule.pr_branches
    in
      (* Store the symbol declaration. *)
      (* A nonterminal symbol is considered public if it is declared using
         %public or %start. *)
      if prule.pr_public
        || StringMap.mem prule.pr_nt grammar.p_start_symbols then
        store_public_nonterminal tokens symbols prule.pr_nt prule.pr_positions
      else
        store_private_nonterminal tokens symbols prule.pr_nt prule.pr_positions
  in
    List.fold_left symbols_of_rule (empty_symbol_table ()) pg.pg_rules

let merge_rules symbols pgs =

  (* Retrieve all the public symbols. *)
  let public_symbols =
    List.fold_left (fold_on_public_symbols (fun s k -> StringSet.add k s))
      (StringSet.singleton "error")
      symbols
  in

  (* We check the references in each grammar can be bound to
     a public symbol. *)
  let _ =
    List.iter
      (iter_on_only_used_symbols
         (fun k pos -> if not (StringSet.mem k public_symbols) then
            Error.error [ pos ]
              "%s is undefined." k))
      symbols
  in
  (* Detect private symbol clashes and rename them if necessary. *)
  let detect_private_symbol_clashes =
    fold_on_private_symbols
      (fun (defined, clashes) symbol ->
         if StringSet.mem symbol defined
           || StringSet.mem symbol public_symbols then
           (defined, StringSet.add symbol clashes)
         else
           (StringSet.add symbol defined, clashes))
  in
  let _private_symbols, clashes =
    List.fold_left detect_private_symbol_clashes (StringSet.empty, StringSet.empty) symbols
  in
  let rpgs = List.map
    (fun (symbol_table, pg) ->
       let renaming =
         StringSet.fold
           (fun x phi ->
              if is_private_symbol symbol_table x then begin
                  let x' = rename x pg.pg_filename in
                    Printf.fprintf stderr
                      "Note: the nonterminal symbol %s (from %s) is renamed %s.\n"
                      x pg.pg_filename x';
                    (x, x') :: phi
                end
              else phi)
           clashes []
       in
         rewrite_grammar renaming pg)
    pgs
  in

    (* Merge public nonterminal definitions
       and copy private nonterminal definitions. Since the clashes between
       private symbols have already been resolved, these copies are safe. *)
    List.fold_left
      (fun rules rpg -> List.fold_left
         (fun rules r ->
            let r =
              try
                let r' = StringMap.find r.pr_nt rules in
                let positions = r.pr_positions @ r'.pr_positions in
                let ra, ra' =
                  List.length r.pr_parameters,
                  List.length r'.pr_parameters
                in
                  (* The arity of the parameterized symbols must be constant.*)
                  if ra <> ra' then
                    Error.error positions
                      "the symbol %s is defined with arities %d and %d."
                         r.pr_nt ra ra'
                  else if r.pr_inline <> r'.pr_inline then
                    Error.error positions
                         "not all definitions of %s are marked %%inline." r.pr_nt
                  else
                    (* We combine the different branches. The parameters
                       could have different names, we rename them with
                       the fresh names assigned earlier (see the next
                       comment). *)
                    let phi = List.combine r.pr_parameters r'.pr_parameters in
                    let rbr = rewrite_branches phi r.pr_branches in
                      { r' with
                          pr_positions = positions;
                          pr_branches  = rbr @ r'.pr_branches;
                          pr_attributes = r.pr_attributes @ r'.pr_attributes;
                      }
              with Not_found ->
                (* We alphaconvert the rule in order to avoid the capture of
                   private symbols coming from another unit. *)
                alphaconvert_rule r.pr_parameters r
            in
              StringMap.add r.pr_nt r rules) rules rpg.pg_rules)
      StringMap.empty rpgs

let empty : grammar =
  {
    p_preludes                = [];
    p_postludes               = [];
    p_parameters              = [];
    p_start_symbols           = StringMap.empty;
    p_types                   = [];
    p_tokens                  = StringMap.empty;
    p_rules                   = StringMap.empty;
    p_on_error_reduce         = [];
    p_grammar_attributes      = [];
    p_symbol_attributes       = [];
  }

(* [join grammar pg] adds the declarations and postludes of the partial
   grammar [pg] to the grammar [grammar]. *)

let join grammar pg =
  grammar
  |> join_declarations pg.pg_declarations
  |> join_postlude pg.pg_postlude

let join pgs =
  let grammar = List.fold_left join empty pgs in
  let symbols = List.map (symbols_of grammar) pgs in
  let tpgs = List.combine symbols pgs in
  let rules = merge_rules symbols tpgs in
  { grammar with p_rules = rules }

end (* Make *)
