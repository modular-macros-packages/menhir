(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(** This module parses the command line. *)

open FrontAPI
open MiddleAPI

include CHECK_GRAMMAR_SETTINGS
include COMMENT_SETTINGS
include CONSTRUCTION_MODE_SETTINGS
include EXN_SETTINGS
include NOPREFIX_SETTINGS
include REPRESENT_SETTINGS
include STRATEGY_SETTINGS
include TOKEN_TYPE_SETTINGS
include TRACE_SETTINGS

(** The back-end that must be used. *)
val backend: [
  | `ReferenceInterpreter
  | `CoqBackend
  | `CodeBackend
  | `TableBackend
  | `NoBackend
]

(** The list of file names that appear on the command line. *)
val filenames: string list

(**If [token_type_only] is true then Menhir stops after generating the
   definition of the type [token]. This corresponds to [--only-tokens]. *)
val token_type_only: bool

(** Whether conflicts should be explained. *)
val explain: bool

(** Whether the automaton should be dumped before conflict resolution. *)
val dump: bool

(** Whether the automaton should be dumped after conflict resolution. *)
val dump_resolved: bool

(** Whether the grammar's reference graph should be dumped. *)
val reference_graph: bool

(** Whether the automaton's graph should be dumped. *)
val automaton_graph: bool

type preprocess_mode =
  | PMNormal
      (** preprocess and continue *)
  | PMOnlyPreprocess of PlainSyntaxPrinter.mode
      (** preprocess, print grammar, stop *)

(** Whether one should stop and print the grammar after joining and
   expanding the grammar. *)
val preprocess_mode: preprocess_mode

type infer_mode =
  (* Perform no type inference. This is the default mode. *)
  | IMNone
  (* Perform type inference by invoking ocamlc directly. *)
  | IMInfer                (** --infer *)
  | IMDependRaw            (** --raw-depend *)
  | IMDependPostprocess    (** --depend *)
  (* Perform type inference by writing a mock .ml file and reading the
     corresponding inferred .mli file. *)
  | IMWriteQuery of string (** --infer-write-query <filename> *)
  | IMReadReply of string  (** --infer-read-reply <filename> *)

(** Whether and how OCaml type inference (for semantic actions and nonterminal
    symbols) should be performed. See the manual for details. *)
val infer: infer_mode

(** Whether one should inline the non terminal definitions marked
    with the %inline keyword. *)
val inline: bool

(** This undocumented flag causes the code to be transformed by
    [Inline] or [StackLangTransform.inline]. It is on by default. *)
val code_inlining: bool

(** How [ocamlc] and [ocamldep] should be invoked. *)
val ocamlc: string
val ocamldep: string

(** How verbose we should be. *)

val logG: int (** diagnostics on the grammar *)

val logA: int (** diagnostics on the automaton *)

val logC: int (** diagnostics on the generated code *)

(** Whether tasks should be timed. *)
val timings: out_channel option

(** The base name that should be used for the files that we create.
   This name can contain a path. *)
val base: string

(**This setting determines Menhir should behave as an interpreter of sentences
   provided by the user on the standard input channel. Two interpretation
   modes exist. In the mode [`Normal], Menhir interprets the sentence and
   indicates whether it is rejected or accepted. (The flag --trace is taken
   into account, so trace messages can be emitted if desired by the user.) In
   this mode, an additional flag indicates whether a concrete syntax tree
   should be displayed after an input is accepted. In the mode [`Error],
   Menhir expects just one input sentence, expects it to trigger an error at
   the very last token, and displays which state was reached. *)
val interpret :
  [ `No
  | `Normal of [`ShowCST | `DoNotShowCST]
  | `Error ]

(**[incremental] determines whether the incremental API must be generated.
   This flag is [true] if and only if the table back-end has been selected. *)
val incremental: bool

(**[inspection] determines whether the inspection API must be generated. This
   API involves GADTs, and requires producing more tables, so this flag is off
   by default. This flag can be [true] only if the table back-end has been
   selected. In other words, [inspection] implies [incremental]. The
   inspection API is an extension of the incremental API. *)
val inspection : bool

(**[unparsing] determines whether the unparsing API must be generated. This
   API offers facilities for constructing and displaying concrete syntax
   trees. This flag can be [true] only if the table back-end has been
   selected. In other words, [unparsing] implies [incremental]. *)
val unparsing : bool

(** Whether the standard menhir library should be used. *)
val no_stdlib : bool

(** Whether to generate a version check for MenhirLib in the generated parser. *)
val coq_no_version_check : bool

(** Whether the coq description must contain completeness proofs. *)
val coq_no_complete : bool

(** Whether the coq backend should ignore types and semantic actions. *)
val coq_no_actions : bool

(** Whether unresolved LR(1) conflicts, useless precedence declarations,
   productions that are never reduced, etc. should be treated as errors. *)
val strict: bool

(** This flag suppresses all warnings about unused precedence levels. *)
val ignore_all_unused_precedence_levels: bool

(** This flag causes Menhir to produce a list of erroneous input sentences.
   Enough sentences are computed to produce exactly one error in every state
   where an error can occur. *)
val list_errors: bool

(** This flag causes Menhir to read the error message descriptions stored in
   [filename] and compile them to OCaml code. *)
val compile_errors: string option

(** If present, this is a pair of .messages files whose contents should
   be compared. *)
val compare_errors: (string * string) option

(** If present, this is a pair of .messages files whose contents should
   be merged. *)
val merge_errors: (string * string) option

(** This flag causes Menhir to read the error message descriptions stored in
   [filename] and re-generate the auto-generated comments, which begin with
   [##]. This allows bringing these comments up to date when the grammar
   evolves. *)
val update_errors: string option

(** This flag causes Menhir to read the error message descriptions stored in
   [filename] and echo the error sentences (and nothing else; no messages,
   no comments). *)
val echo_errors: string option

(** This flag causes Menhir to read the error message descriptions stored in
   [filename] and echo the error sentences, including the concrete syntax
   of each sentence, in an auto-comment. *)
val echo_errors_concrete: string option

(** This flag causes Menhir to produce a [.cmly] file, which contains a
   binary-format description of the grammar and automaton. *)
val cmly: bool

(** This name is used in --coq mode. It appears in the generated Coq file,
   and indicates under what name (or path) the Coq library MenhirLib is
   known. Its default value is [Some "MenhirLib"]. *)
val coq_lib_path: string option

(** This flag tells whether [$i] notation in semantic actions is allowed. *)
type dollars =
  [`DollarsDisallowed | `DollarsAllowed]

(** This flag tells whether [$i] notation in semantic actions is allowed. *)
val dollars: dollars

(**When [random_sentence] is [Some (nt, goal, style)], the user is asking
   Menhir to show a random sentence. The sentence must be generated by the
   nonterminal symbol [nt]. Its goal length is [goal]. The [style] parameter
   indicates whether the sentence should be displayed in concrete syntax; if
   it is [`Concrete], then every token must have a token alias. *)
val random_sentence : (string * int * [`Abstract | `Concrete]) option

(**The setting -O allows choosing an optimization level. It is used only by
   the code back-end. *)
val optimization_level: int

(**The undocumented flag [--stacklang-dump] causes the StackLang program to be
   printed. *)
val stacklang_dump: bool

(**The undocumented flag [--stacklang-graph] causes the StackLang program to be
   dumped in the form of a control flow graph in the file [<basename>.dot]. *)
val stacklang_graph: bool

(**The undocumented flag [--stacklang-test] causes the StackLang program to be
   tested (by comparison with the reference interpreter). *)
val stacklang_test: bool

(**The undocumented flag [--specialize-token] causes the StackLang program to
   be specialized with respect to the current token. This guarantees that
   every token is case-analyzed exactly once, immediately after it has been
   read from the lexer. *)
val specialize_token: bool

(**The undocumented flag [--pack-classic] causes the table back-end to perform
   table compression using the classic row displacement algorithm. Without
   this flag, table compression is performed using the new row displacement
   algorithm. The new algorithm has been found to be significantly faster and
   to produce results of roughly comparable quality. The size of the resulting
   tables can vary, up or down, by up to 5% or 10%. This flag will be removed
   in the future. *)
val pack_classic : bool
