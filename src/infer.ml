(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open PlainSyntax
open PlainSyntaxAccessors
open IL
open ILConstruction
open FrontAPI

module Make
(G : sig val grammar : grammar end)
(X : sig
  include API_SETTINGS
  include BASE_SETTINGS
  include COMMENT_SETTINGS
  include EXN_SETTINGS
  include INFER_SETTINGS
  include TOKEN_TYPE_SETTINGS
end)
= struct
open G

module TokenType =
  TokenType.Make(X)
module Interface =
  Interface.Make(X)

(* -------------------------------------------------------------------------- *)

(* Naming conventions. *)

(* The type variable associated with a nonterminal symbol. Its name begins
   with a prefix which ensures that it begins with a lowercase letter and
   cannot clash with OCaml keywords. *)

let ntvar symbol =
  TypVar (Printf.sprintf "tv_%s" (MString.normalize symbol))

(* The term variable associated with a nonterminal symbol. Its name begins
   with a prefix which ensures that it begins with a lowercase letter and
   cannot clash with OCaml keywords. *)

let encode symbol =
  Printf.sprintf "xv_%s" (MString.normalize symbol)

let decode s =
  let n = String.length s in
  if not (n >= 3 && String.sub s 0 3 = "xv_") then
    Lexmli.fail();
  String.sub s 3 (n - 3)

(* The name of the temporary file. *)

let mlname =
  X.base ^ ".ml"

let mliname =
  X.base ^ ".mli"

(* -------------------------------------------------------------------------- *)

(* The type of a nonterminal symbol. *)

(* 2020/05/25. We must be careful how we process the type information given to
   us by the user via [%type] and [%start] annotations. Duplicating this
   information without care can be unsound: e.g., some OCaml types, such as
   the open polymorphic variant type [> `A ], contain implicit type variables.
   When such a type annotation is duplicated, the two copies are unrelated:
   this can cause us to infer a type that is too general (issue #37).

   To avoid this issue, part of the solution is to enforce sharing by using a
   type variable everywhere (one per nonterminal symbol).

   At first sight, it looks as though we could use this type variable
   everywhere, and use the user-provided type annotation exactly once.
   However, another OCaml feature, type-directed disambiguation, depends on
   explicit type information, and does not always make a correct choice if
   this information is not immediately available (prior to unification).

   Thus, whenever we need to annotate an expression or a pattern with the type
   of a nonterminal symbol, we must annotate it *both* with the type variable
   that corresponds to this symbol (so as to enforce sharing) and with the
   user-provided type, if there is one (so as to allow type-directed
   disambiguation).

   Also, when reading the types inferred by the OCaml type-checker, we must
   read every type, even in the cases where a type was already provided by
   the user. Indeed, the type inferred by OCaml may be more specific than the
   type provided by the user. E.g., the user may declare the type [> `A ],
   while OCaml may infer the type [> `A | `B ]. *)

(* [annotate_expr e nt] annotates the expression [e] with the type information
   associated with the nonterminal symbol [nt]. The expression is annotated
   possibly twice: once with the type variable [ntvar nt] and once with the
   type that the user has possibly provided via a %type declaration. *)

let opt_annotate_expr e (oty : ocamltype option) =
  Option.fold (fun ty e -> annotate e (TypTextual ty)) oty e

let annotate_expr e nt =
  let e = annotate e (ntvar nt) in
  let e = opt_annotate_expr e (ocamltype_of_symbol grammar nt) in
  e

(* [annotate_pat p nt] annotates the pattern [p] with the type information
   associated with the nonterminal symbol [nt]. *)

let opt_annotate_pat p (oty : ocamltype option) =
  Option.fold (fun ty p -> PAnnot (p, TypTextual ty)) oty p

let annotate_pat p nt =
  let p = PAnnot (p, ntvar nt) in
  let p = opt_annotate_pat p (ocamltype_of_symbol grammar nt) in
  p

(* -------------------------------------------------------------------------- *)
(* Code production. *)

(* [tokentype symbol] returns the type of the terminal symbol
   [symbol], if [symbol] is indeed a terminal symbol. Otherwise, it
   raises [Not_found]. *)

let tokentype symbol =
  let props = StringMap.find symbol grammar.tokens in
  match props.tk_ocamltype with
  | None ->
      tunit
  | Some ocamltype ->
      TypTextual ocamltype

(* [actiondef] turns a branch into a function definition. *)

(* The names and types of the conventional internal variables that
   correspond to keywords ($startpos,etc.) are hardwired in this
   code. It would be nice if these conventions were more clearly
   isolated and perhaps moved to the [Action] or [Keyword] module. *)

let actiondef symbol branch =

  (* Construct a list of the semantic action's formal parameters that
     depend on the production's right-hand side. *)

  let formals =
    List.fold_left (fun formals producer ->
      let symbol = producer.prod_symbol
      and lid = producer.prod_id in
      let id = Located.value lid in
      let startp, endp, starto, endo, loc =
        Printf.sprintf "_startpos_%s_" id,
        Printf.sprintf "_endpos_%s_" id,
        Printf.sprintf "_startofs_%s_" id,
        Printf.sprintf "_endofs_%s_" id,
        Printf.sprintf "_loc_%s_" id
      in
      let pid =
        (* We use [PVarLocated] in preference to [PVar] so that this binding
           can be located in the source file (not in the generated file) by
           the OCaml compiler. This is important when a variable declared by
           the user turns out to be unused in a semantic action. We want the
           unused-variable warning (or error) to be correctly located. *)
        pvarlocated lid
      in
      let pid =
        try
          PAnnot (pid, tokentype symbol)
        with Not_found ->
          (* Symbol is a nonterminal. *)
          annotate_pat pid symbol
      in
      pid ::
      PAnnot (PVar startp, tposition) ::
      PAnnot (PVar endp, tposition) ::
      PAnnot (PVar starto, tint) ::
      PAnnot (PVar endo, tint) ::
      PAnnot (PVar loc, tlocation) ::
      formals
    ) [] branch.producers
  in

  (* Extend the list with parameters that do not depend on the
     right-hand side. *)

  let formals =
    PAnnot (PVar "_startpos", tposition) ::
    PAnnot (PVar "_endpos", tposition) ::
    PAnnot (PVar "_endpos__0_", tposition) ::
    PAnnot (PVar "_symbolstartpos", tposition) ::
    PAnnot (PVar "_startofs", tint) ::
    PAnnot (PVar "_endofs", tint) ::
    PAnnot (PVar "_endofs__0_", tint) ::
    PAnnot (PVar "_symbolstartofs", tint) ::
    PAnnot (PVar "_sloc", tlocation) ::
    PAnnot (PVar "_loc", tlocation) ::
    formals
  in

  (* Construct a function definition out of the above bindings and the
     semantic action. *)

  let body = annotate_expr (Action.expr branch.action) symbol in

  match formals with
  | [] ->
      body
  | _ ->
      EFun (formals, body)

(* [program] turns an entire grammar into a test program. *)

let program () =

  (* Turn the grammar into a bunch of function definitions. *)

  let bindings =
    StringMap.fold (fun symbol rule bindings ->
      List.fold_left (fun bindings branch ->
        let binding = (PWildcard, actiondef symbol branch)
        and priority = Action.priority branch.action in
        (priority, binding) :: bindings
      ) bindings rule.branches
    ) grammar.rules []
  in

  (* Grammar productions that come from Menhir's standard library are placed
     first, so that type errors are not reported in them. *)

  let compare (p1, _) (p2, _) = Int.compare p1 p2 in
  let bindings =
    List.sort compare bindings
    |> List.map snd
  in

  (* Create entry points whose types are the unknowns that we are looking for.
     Mentioning just the type variable [ntvar nt] suffices here. *)

  let ps, ts =
    StringMap.fold (fun nt _ (ps, ts) ->
      PVar (encode (MString.normalize nt)) :: ps,
      ntvar nt :: ts
    ) grammar.rules ([], [])
  in

  let def = {
    valpat = PTuple ps;
    valval = ELet (bindings, annotate EBottom (TypTuple ts))
  }
  in

  (* Insert markers to delimit the part of the file that we are
     interested in. These markers are recognized by [Lexmli]. This
     helps skip the values, types, exceptions, etc. that might be
     defined by the prologue or postlogue. *)

  let begindef = {
    valpat = PVar "menhir_begin_marker";
    valval = EIntConst 0
  }
  and enddef = {
    valpat = PVar "menhir_end_marker";
    valval = EIntConst 0
  } in

  (* Issue the test program. We include the definition of the type of
     tokens, because, in principle, the semantic actions may refer to
     it or to its data constructors. *)

  [ SIFunctor (grammar.parameters,
    interface_to_structure (TokenType.tokentypedef grammar) @
    SIFragment grammar.preludes ::
    SIValDefs (false, [ begindef; def; enddef ]) ::
    SIFragment grammar.postludes ::
  [])]

(* -------------------------------------------------------------------------- *)
(* Writing the program associated with a grammar to a file. *)

let write filename () =
  let ml = open_out filename in
  let module P = ILPrinter.Make (struct
    let f = ml
    let print_line_directives = Some filename
    let comment = false
  end) in
  P.program (program());
  close_out ml

(* -------------------------------------------------------------------------- *)
(* Running ocamldep on the program. *)

type entry =
    string (* basename *) * string (* filename *)

type line =
    entry (* target *) * entry list (* dependencies *)

let depend postprocess =

  (* Create an [.ml] file and an [.mli] file, then invoke ocamldep to
     compute dependencies for us. *)

  (* If an old [.ml] or [.mli] file exists, we are careful to preserve
     it. We temporarily move it out of the way and restore it when we
     are done. There is no reason why dependency analysis should
     destroy existing files. *)

  let ocamldep_command =
    Printf.sprintf "%s %s %s"
      X.ocamldep (Filename.quote mlname) (Filename.quote mliname)
  in

  let output : string =
    IO.moving_away mlname @@ fun () ->
    IO.moving_away mliname @@ fun () ->
    IO.with_file mlname (write mlname) @@ fun () ->
    IO.with_file mliname (Interface.write grammar mliname) @@ fun () ->
    IO.invoke_or_die ocamldep_command
  in

  (* Echo ocamldep's output. *)

  print_string output;

  (* If [--raw-depend] was specified on the command line, stop here.  This
     option is used by omake and by ocamlbuild, which performs their own
     postprocessing of [ocamldep]'s output. For normal [make] users, who use
     [--depend], some postprocessing is required, which is performed below. *)

  if postprocess then begin

    (* Make sense out of ocamldep's output. *)

    let lexbuf = Lexing.from_string output in
    let lines : line list =
      try
        Lexdep.main lexbuf
      with Lexdep.Error msg ->
        (* Echo the error message, followed with ocamldep's output. *)
        Report.error' [] "%s" (msg ^ output)
    in

    (* Look for the line that concerns the [.cmo] target, and echo a
       modified version of this line, where the [.cmo] target is
       replaced with [.ml] and [.mli] targets, and where the dependency
       over the [.cmi] file is dropped.

       In doing so, we assume that the user's [Makefile] supports
       bytecode compilation, so that it makes sense to request [bar.cmo]
       to be built, as opposed to [bar.cmx]. This is not optimal, but
       will do. [camldep] exhibits the same behavior. *)

    List.iter (fun ((_, target_filename), dependencies) ->
      if Filename.check_suffix target_filename ".cmo" then
        let dependencies = List.filter (fun (basename, _) ->
          basename <> X.base
        ) dependencies in
        if List.length dependencies > 0 then begin
          Printf.printf "%s %s:" mlname mliname;
          List.iter (fun (_basename, filename) ->
            Printf.printf " %s" filename
          ) dependencies;
          Printf.printf "\n%!"
        end
    ) lines

  end

(* -------------------------------------------------------------------------- *)
(* Augmenting a grammar with inferred type information. *)

(* The parameter [output] is supposed to contain the output of [ocamlc -i]. *)

type env =
  (string * ocamltype) list

let build (output : string) : env =
  try
    Lexmli.main (Lexing.from_string output)
    |> List.map @@ fun (id, openingofs, closingofs) ->
    let ty = String.sub output openingofs (closingofs - openingofs) in
    decode id, Inferred ty
  with Lexmli.Failure ->
    Report.error' [] "failed to make sense of ocamlc's output."

let read_reply (output : string) =
  let env = build output in

  (* Augment the grammar with new %type declarations. *)

  let types =
    StringMap.fold (fun symbol _ types ->
      let ocamltype =
        try
          List.assoc (MString.normalize symbol) env
        with Not_found ->
          (* No type information was inferred for this symbol.
             Perhaps the mock [.ml] file or the inferred [.mli] file
             are out of date. Fail gracefully. *)
          Report.error' [] "found no inferred type for %s." symbol
      in
      (* Regardless of whether there was or wasn't a declared type,
         use the type inferred by the OCaml type-checker. *)
      StringMap.add symbol ocamltype types
    ) grammar.rules grammar.types
  in

  { grammar with types = types }

(* -------------------------------------------------------------------------- *)
(* Inferring types for a grammar's nonterminals. *)

let infer () =

  (* Invoke ocamlc to do type inference for us. *)

  let ocamlc_command =
    Printf.sprintf "%s -c -i %s" X.ocamlc (Filename.quote mlname)
  in

  let output =
    write mlname ();
    let result = IO.invoke_or_die ocamlc_command in
    (* 2015/10/05: intentionally do not remove the [.ml] file if [ocamlc]
       fails. (Or if an exception is thrown.) *)
    Sys.remove mlname;
    result
  in

  (* Make sense out of ocamlc's output. *)

  read_reply output

(* -------------------------------------------------------------------------- *)

let write_query filename =
  write filename ()

(* -------------------------------------------------------------------------- *)

let read_reply filename =
  read_reply (IO.read_whole_file filename)

end (* Make *)
