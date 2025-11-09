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

module TokenType =
  TokenType.Make(Settings)
open TokenType

open NonterminalType

(* -------------------------------------------------------------------------- *)

(* The [Error] exception. *)

let excname =
  "Error"

(* If [fixedexc] is [true] then the exception [Error] is defined as a
   synonym for [Parsing.Parse_error]. Otherwise it is defined as a new
   exception. *)

let exceq =
  if Settings.fixedexc then Some "Parsing.Parse_error" else None

(* If [exn_carries_state] is [true] then the exception [Error] carries an
   integer parameter, a state number. *)

let excparams =
  if Settings.exn_carries_state then [ tint ] else []

let excdef =
  { excname; exceq; excparams }

(* -------------------------------------------------------------------------- *)

(* When the table back-end is active, the generated parser contains,
   as a submodule, an application of [Engine.Make]. This submodule
   is named [Interpreter]. *)

let interpreter =
  interpreter

(* The type [checkpoint] is defined in the incremental API.
   See [IncrementalEngine]. *)

let checkpoint t =
  TypApp (interpreter ^ ".checkpoint", [ t ])

(* -------------------------------------------------------------------------- *)

(* The type of the entry point associated with the start symbol [symbol]
   in the monolithic API. *)

let monolithic_decl grammar symbol =
  let ty = TypTextual (ocamltype_of_start_symbol grammar symbol) in
  let scheme = type2scheme (marrow [ arrow tlexbuf ttoken; tlexbuf ] ty) in
  MString.normalize symbol, scheme

let monolithic_decls grammar =
  StringSet.fold (fun symbol decls ->
    monolithic_decl grammar symbol :: decls
  ) grammar.start_symbols []

(* -------------------------------------------------------------------------- *)

(* The monolithic API contains the type [token], the exception [Error],
   and the monolithic entry points. *)

let monolithic_api grammar =
  tokentypedef grammar @
  IIComment "This exception is raised by the monolithic API functions." ::
  IIExcDecls [ excdef ] ::
  IIComment "The monolithic API." ::
  IIValDecls (monolithic_decls grammar) ::
  []

(* -------------------------------------------------------------------------- *)

(* The type of the entry point associated with the start symbol [symbol]
   in the incremental API. *)

let incremental_decl grammar symbol =
  let ty = TypTextual (ocamltype_of_start_symbol grammar symbol) in
  let scheme = type2scheme (arrow tposition (checkpoint ty)) in
  MString.normalize symbol, scheme

let incremental_decls grammar =
  StringSet.fold (fun symbol decls ->
    incremental_decl grammar symbol :: decls
  ) grammar.start_symbols []

(* -------------------------------------------------------------------------- *)

(* The inspection API. *)

(* This API forms an optional add-on to the incremental API. *)

let inspection_api grammar () =
  let a = "a" in
  (* Definitions of the types [terminal] and [nonterminal]. *)
  tokengadtdef grammar @
  nonterminalgadtdef grammar @
  (* Include the signature [INSPECTION] with suitable type instantiations. *)
  IIComment "The inspection API." ::
  IIInclude (
    with_types WKDestructive
      "MenhirLib.IncrementalEngine.INSPECTION" [
        [ a ], "lr1state", TypApp ("lr1state", [TypVar a]);
        [], "production", TypApp ("production", []);
        [ a ], tctokengadt, ttokengadt (TypVar a);
        [ a ], tcnonterminalgadt, tnonterminalgadt (TypVar a);
        [ a ], "env", TypApp ("env", [ TypVar a ]);
      ]
  ) ::
  []

(* -------------------------------------------------------------------------- *)

(* The incremental API. *)

let incremental =
  "Incremental"

let incremental_engine () : module_type =
  with_types WKNonDestructive
    "MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE"
    [
      [], "token", ttoken
        (* we cannot use [tctoken], which may be qualified *)
    ]

let incremental_entry_points grammar : interface =
  IIComment "The entry point(s) to the incremental API." ::
  IIModule (incremental, MTSigEnd [
    IIValDecls (incremental_decls grammar)
  ]) ::
  []

let incremental_api grammar () : interface =
  IIModule (
    interpreter,
    MTSigEnd (
      IIComment "The incremental API." ::
      IIInclude (incremental_engine()) ::
      MList.ifnlazy Settings.inspection (inspection_api grammar)
    )
  ) ::
  (* The entry points must come after the incremental API,
     because their type refers to the type [checkpoint]. *)
  incremental_entry_points grammar

(* -------------------------------------------------------------------------- *)

(* The unparsing API. *)

let unparsing_api grammar () : interface =
  UnparsingAPI.unparsing_API grammar

(* -------------------------------------------------------------------------- *)

(* The complete interface of the generated parser. *)

let interface grammar = [
  IIFunctor (grammar.parameters,
    monolithic_api grammar @
    MList.ifnlazy (Settings.backend = `TableBackend) (incremental_api grammar) @
    MList.ifnlazy Settings.unparsing (unparsing_api grammar) @
    []
  )
]

(* -------------------------------------------------------------------------- *)

(* Writing the interface to a file. *)

let write grammar mliname () =
  let mli = open_out mliname in
  let module P = ILPrinter.Make (struct
    let f = mli
    let print_line_directives = None
    include Settings
  end) in
  P.interface (interface grammar);
  close_out mli
