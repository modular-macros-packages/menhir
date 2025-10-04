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
open BasicSyntax
open Grammar
let n2i = Nonterminal.n2i
let t2i = Terminal.t2i
let p2i = Production.p2i
open Cmly_format

let ocamltype (typ : Grammar.ocamltype) : ocamltype =
  match typ with
  | Grammar.Declared fragment ->
      value fragment
  | Grammar.Inferred typ ->
      typ

let ocamltype_option (typo : Grammar.ocamltype option) : ocamltype option =
  match typo with
  | None ->
      None
  | Some typ ->
      Some (ocamltype typ)

let range (pos : Range.range) : range =
  {
    r_start = Range.startp pos;
    r_end   = Range.endp pos;
  }

let located l = (value l, range (position l))

let ranges =
  List.map range

let attribute (attr : Attribute.attribute) : attribute =
  {
    a_label    = attr.key;
    a_payload  = attr.payload;
    a_position = range attr.origin;
  }

let attributes : Attribute.attributes -> attributes =
  List.map attribute

let terminal (t : Terminal.t) : terminal_def =
  {
    t_kind = (
      if Terminal.equal t Terminal.error then
        `ERROR
      else if
        (match Terminal.eof with
         | None -> false
         | Some eof -> Terminal.equal t eof) then
        `EOF
      else if Terminal.pseudo t then
        `PSEUDO
      else
        `REGULAR
    );
    t_name = Terminal.print t;
    t_type = ocamltype_option (Terminal.ocamltype t);
    t_attributes = attributes (Terminal.attributes t);
  }

let nonterminal (nt : Nonterminal.t) : nonterminal_def =
  let is_start = Nonterminal.is_internal_start nt in
  {
    n_kind = if is_start then `START else `REGULAR;
    n_name = Nonterminal.print false nt;
    n_mangled_name = Nonterminal.print true nt;
    n_type = if is_start then None else ocamltype_option (Nonterminal.ocamltype nt);
    n_positions = if is_start then [] else ranges (Nonterminal.positions nt);
    n_nullable = Analysis.nullable nt;
    n_first = List.map t2i (TerminalSet.elements (Analysis.first nt));
    n_attributes = if is_start then [] else attributes (Nonterminal.attributes nt);
  }

let symbol (sym : Symbol.t) : symbol =
  match sym with
  | Symbol.N n -> N (n2i n)
  | Symbol.T t -> T (t2i t)

let action (a : Action.t) : action =
  {
    a_expr = ILPrinter.string_of_expr (Action.expr a);
    a_keywords = Keyword.KeywordSet.elements (Action.keywords a);
  }

let rhs (prod : Production.index) : symbol producer_def array =
  match Production.classify prod with
  | Some n ->
      [| (N (n2i n), "", []) |]
  | None ->
      Array.mapi (fun i sym ->
        let id = (Production.identifiers prod).(i) in
        let attrs = attributes (Production.rhs_attributes prod).(i) in
        symbol sym, id, attrs
      ) (Production.rhs prod)

let production (prod : Production.index) : production_def =
  {
    p_kind = if Production.is_start prod then `START else `REGULAR;
    p_lhs = n2i (Production.nt prod);
    p_rhs = rhs prod;
    p_positions = ranges (Production.positions prod);
    p_action = if Production.is_start prod then None
               else Some (action (Production.action prod));
    p_attributes = attributes (Production.attributes prod);
  }

let item (i : Item.t) : production * int =
  let p, i = Item.export i in
  (p2i p, i)

let itemset (is : Item.Set.t) : (production * int) list =
  List.map item (Item.Set.elements is)

let lr0_state (node : Lr0.node) : lr0_state_def =
  {
    lr0_incoming = Option.map symbol (Lr0.incoming_symbol node);
    lr0_items = itemset (Lr0.items node)
  }

let transition (sym, node) : symbol * lr1 =
  (symbol sym, Lr1.number node)

let lr1_state (node : Lr1.node) : lr1_state_def =
  {
    lr1_lr0 = Lr0.core (Lr1.state node);
    lr1_transitions =
      List.map transition (SymbolMap.bindings (Lr1.transitions node));
    lr1_reductions = (
      let add t ps rs = (t2i t, p2i (List.hd ps)) :: rs in
      TerminalMap.fold_rev add (Lr1.reductions node) []
    );
    lr1_default_reduction =
      Option.map (fun (p, _ts) -> p2i p) (Default.has_default_reduction node);
    lr1_conflicting_transitions = (
      let prepare (t, node, silent) = (t2i t, Lr1.number node, silent) in
      List.map prepare (Lr1.get_conflicting_transitions node);
    );
    lr1_conflicting_reductions = (
      let prepare (t, prods, silent) = (t2i t, List.map p2i prods, silent) in
      List.map prepare (Lr1.get_conflicting_reductions node);
    );
    lr1_extra_reductions = (
      let prepare (t, prod) = (t2i t, p2i prod) in
      List.map prepare (Lr1.get_extra_reductions node);
    );
  }

let entry_point prod node nt _typ accu : (nonterminal * production * lr1) list =
  (n2i nt, p2i prod, Lr1.number node) :: accu

let priority_level input level = {
  pl_input_file = InputFile.name input;
  pl_level = level;
}

let precedence_level pl =
  match pl with
  | Syntax.UndefinedPrecedence ->
      None
  | Syntax.PrecedenceLevel il ->
      let input, level = value il in
      Some (priority_level input level, range (position il))

let production_level (Syntax.ProductionLevel (input, level)) =
  priority_level input level

let associativity = function
  | Syntax.LeftAssoc      -> LeftAssoc
  | Syntax.RightAssoc     -> RightAssoc
  | Syntax.NonAssoc       -> NonAssoc
  | Syntax.UndefinedAssoc -> UndefinedAssoc

let import_bindings f map =
  List.map (fun (k, v) -> (k, f v)) (StringMap.bindings map)

let token (tk : Syntax.properties) = {
  tk_ocamltype = ocamltype_option tk.Syntax.tk_ocamltype;
  tk_position = range tk.Syntax.tk_position;
  tk_alias = tk.Syntax.tk_alias;
  tk_attributes = attributes tk.Syntax.tk_attributes;
  tk_associativity = associativity tk.Syntax.tk_associativity;
  tk_precedence = precedence_level tk.Syntax.tk_precedence;
  tk_is_declared = tk.Syntax.tk_is_declared;
}

let basic_producer (pr : BasicSyntax.producer) =
  (pr.BasicSyntax.producer_symbol,
   value pr.BasicSyntax.producer_identifier,
   attributes pr.BasicSyntax.producer_attributes)

let basic_branch (br : BasicSyntax.branch) = {
  br_position         = range br.BasicSyntax.branch_position;
  br_producers        = List.map basic_producer br.BasicSyntax.producers;
  br_action           = action br.BasicSyntax.action;
  br_prec_annotation  = Option.map located br.prec_annotation;
  br_production_level = production_level br.BasicSyntax.production_level;
  br_attributes       = attributes br.BasicSyntax.br_attributes;
}

let basic_rule (rule : BasicSyntax.rule) = {
  r_parameters = ();
  r_branches   = List.map basic_branch rule.BasicSyntax.branches;
  r_inline     = rule.BasicSyntax.inline_flag;
  r_positions  = List.map range rule.BasicSyntax.positions;
  r_public     = false;
  r_attributes = attributes rule.BasicSyntax.attributes;
}

let basic_syntax (g : BasicSyntax.grammar) : ground_syntax = {
  s_types  = import_bindings ocamltype g.types;
  s_tokens = import_bindings token g.tokens;
  s_rules  = import_bindings basic_rule g.rules;
}

let rec higher_branch (br : Syntax.parameterized_branch) = {
  br_position         = range br.Syntax.pb_position;
  br_producers        = List.map higher_producer br.Syntax.pb_producers;
  br_action           = action br.Syntax.pb_action;
  br_prec_annotation  = Option.map located br.Syntax.pb_prec_annotation;
  br_production_level = production_level br.Syntax.pb_production_level;
  br_attributes       = attributes br.Syntax.pb_attributes;
}

and parameter = function
  | Syntax.ParameterVar sym ->
    ParameterVar (located sym)
  | Syntax.ParameterApp (sym, args) ->
    ParameterApp (located sym, List.map parameter args)
  | Syntax.ParameterAnonymous brs ->
    ParameterAnonymous (located (map (List.map higher_branch) brs))

and higher_producer (id, param, attr) =
  (parameter param, value id, attributes attr)

let higher_rule (r : Syntax.parameterized_rule) = {
  r_parameters = r.Syntax.pr_parameters;
  r_branches   = List.map higher_branch r.Syntax.pr_branches;
  r_inline     = r.Syntax.pr_inline_flag;
  r_positions  = List.map range r.Syntax.pr_positions;
  r_public     = r.Syntax.pr_public_flag;
  r_attributes = attributes r.Syntax.pr_attributes;
}

let higher_syntax (g : Syntax.grammar) : higher_syntax = {
  s_types  = (
    let import_type (p,t) = (parameter p, ocamltype (value t)) in
    List.map import_type g.Syntax.p_types;
  );
  s_tokens = import_bindings token g.Syntax.p_tokens;
  s_rules  = import_bindings higher_rule g.Syntax.p_rules;
}

let encode () : grammar =
  {
    g_basename     = Settings.base;
    g_preludes     = List.map value Front.grammar.preludes;
    g_postludes    = List.map value Front.grammar.postludes;
    g_terminals    = Terminal.init terminal;
    g_nonterminals = Nonterminal.init nonterminal;
    g_productions  = Production.init production;
    g_lr0_states   = Array.init Lr0.n lr0_state;
    g_lr1_states   = Array.of_list (Lr1.map lr1_state);
    g_entry_points = Lr1.fold_entry entry_point [];
    g_attributes   = attributes Analysis.attributes;
    g_parameters   = List.map value Front.grammar.parameters;
    g_start_symbols = StringSet.elements Front.grammar.start_symbols;
    g_on_error_reduce =
      List.map (fun (sym, lvl) -> (sym, production_level lvl))
        (StringMap.bindings Front.grammar.on_error_reduce);
    g_before_expansion = higher_syntax Front.grammar_before_expansion;
    g_before_inlining = basic_syntax Front.grammar_before_inlining;
  }

let write oc t =
  (* .cmly file format: CMLY ++ version string ++ grammar *)
  let magic = "CMLY" ^ Version.version in
  output_string oc magic;
  output_value oc (t : grammar)

let write filename =
  (* Opening in binary mode is required. This is not a text file;
     we write to it using [output_value]. *)
  let oc = open_out_bin filename in
  write oc (encode());
  close_out oc
