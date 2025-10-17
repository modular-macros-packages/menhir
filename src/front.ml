(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* The front-end. This module performs a series of toplevel side effects. *)

(* ------------------------------------------------------------------------- *)

(* Reading a grammar from a file. *)

let load_grammar_from_contents filename content =
  InputFile.with_file_content filename content @@ fun lexbuf ->
  { (Driver.grammar Lexer.main lexbuf)
    with Syntax.pg_filename = filename }

let load_grammar_from_file filename : Syntax.partial_grammar =
  Lexer.priority := 1;
  try
    let contents = IO.read_whole_file filename in
    load_grammar_from_contents filename contents
  with Sys_error msg ->
    Error.error [] "%s" msg

(* ------------------------------------------------------------------------- *)

(* Read all of the grammar files that are named on the command line, plus the
   standard library, unless suppressed by [--no-stdlib] or [--coq]. *)

let grammars () : Syntax.partial_grammar list =
  List.map load_grammar_from_file Settings.filenames

let grammars : Syntax.partial_grammar list =
  if Settings.no_stdlib || Settings.backend = `CoqBackend then
    grammars()
  else
    (* As 20190924, the standard library is no longer actually read from a
       file. Instead, its text is built into the Menhir executable: it is
       found in the string [Standard_mly.contents]. We parse it just as if
       it had been read from a file, and pretend that the file name is
       "<standard.mly>". This file name can appear in generated
       parsers, because Menhir produces # directives that point back to
       source (.mly) files. *)
    (* Note that the [let] construct below is required in order to ensure
       that the standard library is read first. *)
    let standard_library =
      Lexer.priority := 0;
      load_grammar_from_contents
        "<standard.mly>"
        Standard_mly.contents
    in
    standard_library :: grammars()

let () =
  Time.tick "Lexing and parsing"

(* ------------------------------------------------------------------------- *)

(* Eliminate anonymous rules. *)

let grammars : Syntax.partial_grammar list =
  List.map EliminateAnonymousRules.transform grammars

(* Remove all uses of token aliases. *)

let grammars =
  let module E = ExpandTokenAliases.Make(Error) in
  E.transform grammars

(* ------------------------------------------------------------------------- *)

(* If several grammar files were provided on the command line, merge them.   *)

(* Then, check that the resulting grammar is well-formed. *)

let grammar : Syntax.grammar =

  let c = Error.new_category() in
  let module E = struct
    include Error
    let signal format = Error.signal c format
    (* For each symbol [s], we report at most one error about this symbol. *)
    let can_complain_about : string -> bool =
      let reported = ref StringSet.empty in
      fun symbol ->
        not (StringSet.mem symbol !reported) &&
        let () = reported := StringSet.add symbol !reported in
        true
  end in

  let module J = JoinGrammars.Make(E) in
  let grammar = J.join grammars in
  (* We do NOT exit here if an error has been signaled. Thus we allow
     CheckGrammar to run: this allows us to signal even more errors
     and to blur the distinction between JoinGrammars and CheckGrammars
     as far as the user is concerned. *)
  (* Error.exit_if c; *)

  let module X = struct
    let ignore_unused_token t =
      Settings.ignore_all_unused_tokens ||
      Settings.token_type_mode = Settings.TokenTypeOnly ||
      StringSet.mem t Settings.ignored_unused_tokens
  end in
  let module C = CheckGrammar.Make(E)(X) in
  C.check grammar;
  Error.exit_if c;

  grammar

(* ------------------------------------------------------------------------- *)

(* Check that the grammar is well-sorted; infer the sort of every symbol. *)

let sorts =
  let module V = struct
    let verbose = Settings.logG >= 3
  end in
  let module S = SortInference.Make(Error)(V) in
  S.infer grammar

(* ------------------------------------------------------------------------- *)

(* This is the grammar before expansion. *)

let grammar_before_expansion =
  grammar

(* ------------------------------------------------------------------------- *)

(* Expand away all applications of parameterized nonterminal symbols, so as
   to obtain a grammar without parameterized nonterminal symbols. *)

let grammar : PlainSyntax.grammar =
  let open SelectiveExpansion in
  (* First, perform a selective expansion: expand away all parameters of
     higher sort, keeping the parameters of sort [*]. This process always
     terminates. *)
  let grammar1 = expand ExpandHigherSort sorts grammar in
  (* This "first-order parameterized grammar" can then be submitted to
     the termination check. *)
  let module C = CheckSafeExpansion.Make(Error) in
  C.check grammar1;
  (* If it passes the check, then full expansion is safe. We drop [grammar1]
     and start over from [grammar]. This is required in order to get correct
     names. (Expanding [grammar1] would yield an equivalent grammar, with
     more complicated names, reflecting the two steps of expansion.) *)
  let grammar = expand ExpandAll sorts grammar in
  (* This yields an unparameterized grammar. *)
  Drop.drop grammar

let () =
  Time.tick "Joining and expanding"

(* ------------------------------------------------------------------------- *)

(* If [--strategy simplified] has been selected, then check that the [error]
   token is used only at the end of productions (2021/10/31). *)

let grammar =
  CheckErrorTokenUsage.filter_grammar grammar

(* ------------------------------------------------------------------------- *)

(* If [--only-tokens] was specified on the command line, produce
   the definition of the [token] type and stop. *)

let () =
  match Settings.token_type_mode with
  | Settings.TokenTypeOnly ->
      TokenType.produce_tokentypes grammar;
      exit 0
  | Settings.CodeOnly _
  | Settings.TokenTypeAndCode ->
      ()

(* ------------------------------------------------------------------------- *)

(* Perform reachability analysis. *)

let grammar =
  let module E = struct let warning = Error.grammar_warning end in
  let module T = Reachability.Make(E) in
  T.trim grammar

let () =
  Time.tick "Trimming"

(* ------------------------------------------------------------------------- *)

(* If [--infer] was specified on the command line, perform type inference.
   The OCaml type of every nonterminal symbol is then known. *)

(* If [--depend] or [--raw-depend] was specified on the command line,
   perform dependency analysis and stop. *)

(* The purpose of [--depend] and [--raw-depend] is to support [--infer].
   Indeed, [--infer] is implemented by producing a mock [.ml] file (which
   contains just the semantic actions) and invoking [ocamlc]. This requires
   certain [.cmi] files to exist. So, [--(raw-)depend] is a way for us to
   announce which [.cmi] files we need. It is implemented by producing the
   mock [.ml] file and running [ocamldep] on it. We also produce a mock
   [.mli] file, even though in principle it should be unnecessary -- see
   comment in [nonterminalType.mli]. *)

(* If [--infer-write-query] was specified on the command line, write a
   mock [.ml] file and stop. It is then up to the user (or build system)
   to invoke [ocamlc -i] on this file, so as to do type inference. *)

(* If [--infer-read-reply] was specified on the command line, read the
   inferred [.mli] file. The OCaml type of every nonterminal symbol is
   then known, just as with [--infer]. *)

let grammar, ocaml_types_have_been_checked =
  Settings.(match infer with
  | IMNone ->
      grammar, false
  | IMInfer ->
      let grammar = Infer.infer grammar in
      Time.tick "Inferring types for nonterminals";
      grammar, true
  | IMDependRaw ->
      Infer.depend false grammar         (* never returns *)
  | IMDependPostprocess ->
      Infer.depend true grammar          (* never returns *)
  | IMWriteQuery filename ->
      Infer.write_query filename grammar (* never returns *)
  | IMReadReply filename ->
      let grammar = Infer.read_reply filename grammar in
      Time.tick "Reading inferred types for nonterminals";
      grammar, true
  )

(* ------------------------------------------------------------------------- *)

(* Expand away some of the position keywords. *)

let grammar =
  KeywordExpansion.expand_grammar grammar

(* ------------------------------------------------------------------------- *)

(* If [--no-inline] was specified on the command line, skip the
   inlining of non terminal definitions marked with %inline. *)

let grammar_before_inlining =
  grammar

let grammar =
  if Settings.inline then begin
    let grammar = Inlining.inline grammar in
    (* 2018/05/23 Removed the warning that was issued when %inline was used
       but --infer was turned off. Most people should use ocamlbuild or dune
       anyway. *)
    Time.tick "Inlining";
    grammar
  end
  else
    grammar

(* ------------------------------------------------------------------------- *)

(* If [--only-preprocess] or [--only-preprocess-drop] was specified on the
   command line, print the grammar and stop. Otherwise, continue. *)

let () =
  match Settings.preprocess_mode with
  | Settings.PMOnlyPreprocess mode ->
      PlainSyntaxPrinter.print mode stdout grammar;
      exit 0
  | Settings.PMNormal ->
      ()
