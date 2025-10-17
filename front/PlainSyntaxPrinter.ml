(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open Located
open Attribute
open PlainSyntax
open PlainSyntaxAccessors

(* When the original grammar is split over several files, it may be IMPOSSIBLE
   to print it out into a single file, as that would introduce a total ordering
   (between rules, between priority declarations, between %on_error_reduce
   declarations) that did not exist originally. We currently do not warn about
   this problem. Nobody has ever complained about it. We use the function
   [InputFile.compare], whose use is discouraged, in order to obtain a total
   order on precedence levels, and we obey this order when printing. *)

type mode =
  | PrintNormal
  | PrintForOCamlyacc
  | PrintUnitActions of bool

module Print (X : sig val mode : mode end) = struct
open X

(* -------------------------------------------------------------------------- *)

(* Printing an OCaml type. *)

let print_ocamltype ty : string =
  sprintf " <%s>" @@
    match ty with
    | Declared fragment ->
        value fragment
    | Inferred t ->
        t

let print_ocamltype ty : string =
  let s = print_ocamltype ty in
  match mode with
  | PrintForOCamlyacc ->
      (* ocamlyacc does not allow a %type declaration to contain
         a new line. Replace it with a space. *)
      String.map (function '\r' | '\n' -> ' ' | c -> c) s
  | PrintNormal
  | PrintUnitActions _ ->
      s

(* -------------------------------------------------------------------------- *)

(* Printing the type of a terminal symbol. *)

let print_token_type (prop : properties) =
  match prop.tk_ocamltype, mode with
  | None   , _
  | Some  _, PrintUnitActions true ->
      (* When %token is not followed by a type,
         this means that the type is [unit]. *)
      ""
  | Some ty, PrintNormal
  | Some ty, PrintForOCamlyacc
  | Some ty, PrintUnitActions false ->
      print_ocamltype ty

(* -------------------------------------------------------------------------- *)

(* Printing the type of a nonterminal symbol. *)

let print_nonterminal_type ty =
  match mode with
  | PrintNormal
  | PrintForOCamlyacc ->
      print_ocamltype ty
  | PrintUnitActions _ ->
      " <unit>"

(* -------------------------------------------------------------------------- *)

(* Printing a binding for a semantic value. *)

let print_binding id =
  match mode with
  | PrintNormal ->
      id ^ " = "
  | PrintForOCamlyacc
  | PrintUnitActions _ ->
      (* need not, or must not, bind a semantic value *)
      ""

(* -------------------------------------------------------------------------- *)

(* Testing whether it is permitted to print OCaml code
   (semantic actions, prelude, postlude). *)

let if_ocaml_code_permitted f x =
  match mode with
  | PrintNormal
  | PrintForOCamlyacc ->
      f x
  | PrintUnitActions _ ->
      (* In these modes, all OCaml code is omitted: semantic actions,
         preludes, postludes, etc. *)
      ()

(* -------------------------------------------------------------------------- *)

(* Testing whether attributes should be printed. *)

let attributes_printed : bool =
  match mode with
  | PrintNormal
  | PrintUnitActions _ ->
      true
  | PrintForOCamlyacc ->
      false

(* -------------------------------------------------------------------------- *)

(* Printing a semantic action. *)

let print_semantic_action f g branch =
  let e = Action.expr branch.action in
  match mode with
  | PrintUnitActions _ ->
      (* In the unit-action modes, we print a pair of empty braces. *)
      ()
  | PrintNormal ->
      ILPrinter.print_expr f e
  | PrintForOCamlyacc ->
       (* In ocamlyacc-compatibility mode, the code must be wrapped in
          [let]-bindings whose right-hand side uses the [$i] keywords. *)
      let bindings =
        List.mapi (fun i producer ->
          let id = prod_id producer
          and symbol = producer.prod_symbol in
          (* Test if [symbol] is a terminal symbol whose type is [unit]. *)
          let is_unit_token =
            try
              let prop = StringMap.find symbol g.tokens in
              prop.tk_ocamltype = None
            with Not_found ->
              symbol = "error"
          in
          (* Define the variable [id] as a synonym for [$(i+1)]. *)
          (* As an exception to this rule, if [symbol] is a terminal symbol
             which has been declared *not* to carry a semantic value, then
             we cannot use [$(i+1)] -- ocamlyacc does not allow it -- so we
             use the unit value instead. *)
          IL.PVar id,
          if is_unit_token then
            IL.EUnit
          else
            IL.EVar (sprintf "$%d" (i + 1))
        ) branch.producers
      in
      (* The identifiers that we bind are pairwise distinct. *)
      (* We must use simultaneous bindings (that is, a [let/and] form), as
          opposed to a cascade of [let] bindings. Indeed, ocamlyacc internally
          translates [$i] to [_i] (just like us!), so name captures will occur
          unless we restrict the use of [$i] to the outermost scope. (Reported
          by Kenji Maillard.) *)
      let e = ILConstruction.eletand (bindings, e) in
      ILPrinter.print_expr f e

(* -------------------------------------------------------------------------- *)

(* Printing preludes and postludes. *)

let print_preludes f g =
  g.preludes |> List.iter @@ fun prelude ->
  fprintf f "%%{%s%%}\n" (value prelude)

let print_postludes f g =
  g.postludes |> List.iter @@ fun postlude ->
  fprintf f "%s\n" (value postlude)

(* -------------------------------------------------------------------------- *)

(* Printing %start declarations. *)

let print_start_symbols f g =
  g.start_symbols |> StringSet.iter @@ fun symbol ->
  fprintf f "%%start %s\n" (MString.normalize symbol)

(* -------------------------------------------------------------------------- *)

(* Printing %parameter declarations. *)

let print_parameter f parameter =
  fprintf f "%%parameter<%s>\n" (value parameter)

let print_parameters f g =
  match mode with
  | PrintNormal ->
      List.iter (print_parameter f) g.parameters
  | PrintForOCamlyacc
  | PrintUnitActions _ ->
       (* %parameter declarations are not supported by ocamlyacc,
          and presumably become useless when the semantic actions
          are removed. *)
      ()

(* -------------------------------------------------------------------------- *)

(* Printing attributes. *)

let print_attribute f { key; payload; origin } =
  ignore origin;
  if attributes_printed then
    fprintf f " [@%s %s]" key payload

let print_attributes f attrs =
  List.iter (print_attribute f) attrs

(* -------------------------------------------------------------------------- *)

(* Printing token declarations and precedence declarations. *)

let print_assoc = function
  | LeftAssoc ->
      sprintf "%%left"
  | RightAssoc ->
      sprintf "%%right"
  | NonAssoc ->
      sprintf "%%nonassoc"
  | UndefinedAssoc ->
      ""

let compare_pairs compare1 compare2 (x1, x2) (y1, y2) =
  let c = compare1 x1 y1 in
  if c <> 0 then c
  else compare2 x2 y2

let compare_tokens (_token, prop) (_token', prop') =
  match prop.tk_precedence, prop'.tk_precedence with
  | UndefinedPrecedence, UndefinedPrecedence ->
      0
  | UndefinedPrecedence, PrecedenceLevel _ ->
      -1
  | PrecedenceLevel _, UndefinedPrecedence ->
      1
  | PrecedenceLevel mv, PrecedenceLevel mv' ->
      compare_pairs InputFile.compare Int.compare (value mv) (value mv')

let print_token_declarations f g =
  g.tokens |> StringMap.iter @@ fun token prop ->
  if prop.tk_is_declared then
    fprintf f "%%token%s %s%a\n"
      (print_token_type prop)
      token
      print_attributes prop.tk_attributes

let print_precedence_declarations f g =
  (* Sort the tokens wrt. precedence, and group them into levels. *)
  let levels = MList.group compare_tokens (StringMap.bindings g.tokens) in
  (* Print the precedence declarations: %left, %right, %nonassoc. *)
  levels |> List.iter @@ fun (level : (string * properties) list) ->
  let (_token, prop) = List.hd level in
  (* Do nothing about the tokens that have no precedence. *)
  if prop.tk_precedence <> UndefinedPrecedence then begin
    fprintf f "%s" (print_assoc prop.tk_associativity);
    List.iter (fun (token, _prop) ->
      fprintf f " %s" token
    ) level;
    fprintf f "\n"
  end

let print_tokens f g =
  print_token_declarations f g;
  print_precedence_declarations f g

(* -------------------------------------------------------------------------- *)

(* Printing %type declarations. *)

let print_types f g =
  g.types |> StringMap.iter @@ fun symbol ty ->
  fprintf f "%%type%s %s\n"
    (print_nonterminal_type ty)
    (MString.normalize symbol)

(* -------------------------------------------------------------------------- *)

(* Printing branches and rules. *)

let print_producer sep f producer =
  fprintf f "%s%s%s%a"
    (sep())
    (print_binding (prod_id producer))
    (MString.normalize producer.prod_symbol)
    print_attributes producer.prod_attributes

let print_branch f g branch =
  (* Print the producers. *)
  let sep = MString.this_then_that "" " " in
  List.iter (print_producer sep f) branch.producers;
  (* Print the %prec annotation, if there is one. *)
  Option.iter (fun x ->
    fprintf f " %%prec %s" (value x)
  ) branch.prec_annotation;
  (* Newline, indentation, semantic action. *)
  fprintf f "\n    {";
  print_semantic_action f g branch;
  fprintf f "}";
  (* The branch attributes follow the semantic action. *)
  print_attributes f branch.br_attributes;
  fprintf f "\n"

(* Because the resolution of reduce/reduce conflicts is implicitly dictated by
   the order in which productions appear in the grammar, the printer should be
   careful to preserve this order. *)

(* 2016/08/25: As noted above, when two productions originate in different files,
   we have a problem. We MUST print them in some order, even though they should
   be incomparable. In that case, we use the order in which the source files are
   specified on the command line. However, this behavior is undocumented, and
   should not be exploited. (In previous versions of Menhir, the function passed
   to [List.sort] was not transitive, so it did not make any sense!) *)

let compare_production_levels bpl bpl' =
  match bpl, bpl' with
  | ProductionLevel (m, l), ProductionLevel (m', l') ->
      compare_pairs InputFile.compare Int.compare (m, l) (m', l')

let compare_branches (b : branch) (b' : branch) =
  compare_production_levels b.production_level b'.production_level

let compare_rules (_nt, (r : rule)) (_nt', (r' : rule)) =
  match r.branches, r'.branches with
  | [], [] ->
      0
  | [], _ ->
      -1
  | _, [] ->
      1
  | b :: _, b' :: _ ->
      (* To compare two rules, it suffices to compare their first productions. *)
      compare_branches b b'

let print_rule f g (nt, r) =
  fprintf f "\n%s%a:\n" (MString.normalize nt) print_attributes r.attributes;
  (* Menhir accepts a leading "|", but bison does not. Let's not print it.
     So, we print a bar-separated list. *)
  let sep = MString.this_then_that ("  ") ("| ") in
  r.branches |> List.iter @@ fun br ->
  fprintf f "%s" (sep());
  print_branch f g br

let print_rules f g =
  let rules = List.sort compare_rules (StringMap.bindings g.rules) in
  List.iter (print_rule f g) rules

(* -------------------------------------------------------------------------- *)

(* Printing %on_error_reduce declarations. *)

let print_on_error_reduce_declarations f g =
  let cmp (_nt, oel) (_nt', oel') = compare_production_levels oel oel' in
  MList.group cmp (StringMap.bindings g.on_error_reduce)
  |> List.iter @@ fun level ->
  fprintf f "%%on_error_reduce";
  List.iter (fun (nt, _level) ->
    fprintf f " %s" (MString.normalize nt)
  ) level;
  fprintf f "\n"

let print_on_error_reduce_declarations f g =
  match mode with
  | PrintNormal
  | PrintUnitActions _ ->
      print_on_error_reduce_declarations f g
  | PrintForOCamlyacc ->
      (* %on_error_reduce declarations are not supported by ocamlyacc *)
      ()

(* -------------------------------------------------------------------------- *)

(* Printing %attribute declarations. *)

let print_grammar_attribute f { key; payload; origin } =
  ignore origin;
  if attributes_printed then
    fprintf f "%%[@%s %s]\n" key payload

let print_grammar_attributes f g =
  List.iter (print_grammar_attribute f) g.gr_attributes

(* -------------------------------------------------------------------------- *)

(* The main entry point. *)

let print f g =
  print_parameters f g;
  if_ocaml_code_permitted (print_preludes f) g;
  print_start_symbols f g;
  print_tokens f g;
  print_types f g;
  print_on_error_reduce_declarations f g;
  print_grammar_attributes f g;
  fprintf f "%%%%\n";
  print_rules f g;
  fprintf f "\n%%%%\n";
  if_ocaml_code_permitted (print_postludes f) g

end

let print mode =
  let module P = Print(struct let mode = mode end) in
  P.print
