(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open PlainSyntaxAccessors
open IL

(* This is the conventional name of the nonterminal GADT, which describes the
   nonterminal symbols. *)

let tcnonterminalgadt =
  "nonterminal"

let tnonterminalgadt a =
  TypApp (tcnonterminalgadt, [ a ])

(* This is the conventional name of the data constructors of the nonterminal
   GADT. *)

let tnonterminalgadtdata nt =
  "N_" ^ MString.normalize nt

(* This is the definition of the nonterminal GADT. Here, the data
   constructors have no value argument, but have a type index. *)

(* Generating this definition requires the type of every nonterminal symbol to
   be known. We assume that this check has been carried out up front. This
   said, when [--depend] or [--raw-depend] is used, [nonterminalgadtdef] can
   be invoked by [Infer.depend] via [Interface.write]. Then, we may encounter
   a nonterminal symbol whose type is not known. In that case, we give up and
   define ['a nonterminal] as an abstract type.*)

exception MissingOCamlType of string

let nonterminalgadtdef grammar =
  let comment, datadefs =
    try

      (* The ordering of this list matters. We want the data constructors
         to respect the internal ordering (as determined by [nonterminals]
         in [PlainSyntax]) of the nonterminal symbols. This may
         be exploited in the table back-end to allow an unsafe conversion
         of a data constructor to an integer code. See [n2i] in
         [InspectionTableInterpreter]. *)

      "The indexed type of nonterminal symbols.",
      List.map (fun nt ->
        let index =
          match ocamltype_of_symbol grammar nt with
          | Some t ->
              TypTextual t
          | None ->
              raise (MissingOCamlType nt)
        in
        {
          dataname = tnonterminalgadtdata nt;
          datavalparams = [];
          datatypeparams = Some [ index ];
          comment = None;
          unboxed = false;
        }
      ) (nonterminals grammar)

    with MissingOCamlType _nt ->
      (* Give up and define ['a nonterminal] as an abstract type. This is
         useful when we are in [--(raw)-depend] mode. Then, we produce a mock
         [.mli] file that is an approximation of the real [.mli] file. *)
      "The indexed type of nonterminal symbols (mock!).",
      []

  in
  [
    IIComment comment;
    IITypeDecls [{
      typename = tcnonterminalgadt;
      typeparams = [ "_" ];
      typerhs = TDefSum datadefs;
      typeconstraint = None
    }]
  ]
