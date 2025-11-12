(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open IL
open ILConstruction
open FrontAPI
open MiddleAPI

module Make (G : GRAMMAR) (X : sig

  include TOKEN_TYPE_SETTINGS

  (**If [represent_values] is [true] then every stack cell is forced to
     contain a semantic value. If it is [false] then a stack cell that
     corresponds to a terminal symbol whose OCaml type is [unit] does not
     contain a semantic value. *)
  val represent_values: bool

 (**If [noprefix] is [false] then most identifiers begin with an unlikely
    prefix, in order to avoid name collisions. If [noprefix] is [true] then
    this prefix is removed. This makes the code more readable but can cause
    name collisions. *)
  val noprefix: bool

end) = struct
open G

module TokenType =
  TokenType.Make(X)
module ILTokens =
  ILTokens.Make(G)(X)
open ILTokens

(* -------------------------------------------------------------------------- *)

(* Prefixes. *)

let prefix name =
  if X.noprefix then
    name
  else
    (* This prefix must begin with an underscore. This allows avoiding
       warnings about unused variables with OCaml 3.09 and later. *)
    "_menhir_" ^ name

let dataprefix name =
  if X.noprefix then
    name
  else
    (* This prefix must begin with a capital letter. *)
    "Menhir" ^ name

let tvprefix name =
  if X.noprefix then
    name
  else
    (* This prefix must begin with an ordinary letter (not an underscore). *)
    "ttv_" ^ name

(* -------------------------------------------------------------------------- *)

(* Naming conventions. *)

let ntvar nt =
  TypVar (Printf.sprintf "tv_%s" (Nonterminal.print true nt))

let semv =
  "_v"

let stack =
  prefix "stack"

let state =
  prefix "s"

let token =
  "_tok"

(* The following variables are used to hold start and end positions. Do not
   change these names! They are chosen to coincide with the $startpos and
   $endpos keywords, which the lexer rewrites to _startpos and _endpos, so
   that binding these variables before executing a semantic action is
   meaningful. *)

(* These names should agree with the printing function [Keyword.posvar]. *)

let beforeendp =
  Keyword.(posvar Before WhereEnd FlavorPosition)
    (* "_endpos__0_" *)

let startp =
  Keyword.(posvar Left WhereStart FlavorPosition)
    (* "_startpos" *)

let endp =
  Keyword.(posvar Left WhereEnd FlavorPosition)
    (* "_endpos" *)

let startpos ids i =
  Keyword.(posvar (RightNamed ids.(i)) WhereStart FlavorPosition)
    (* sprintf "_startpos_%s_" ids.(i) *)

let endpos ids i =
  Keyword.(posvar (RightNamed ids.(i)) WhereEnd FlavorPosition)
    (* sprintf "_endpos_%s_" ids.(i) *)

(* -------------------------------------------------------------------------- *)

(* Types for semantic values. *)

let semvtype nt =
  match Nonterminal.ocamltype nt with
  | None ->
      (* [nt] has unknown type. If we have performed type inference, this
         cannot happen. However, type inference is still optional, so we
         must tolerate this. *)
      ntvar nt
  | Some ocamltype ->
      TypTextual ocamltype

let semvtypetok tok =
  match Terminal.ocamltype tok with
  | None ->
      (* This terminal symbol has type [unit], so it is omitted in every
         stack cell, unless [X.represent_values] is true. *)
      if X.represent_values then [ tunit ] else []
  | Some ocamltype ->
      [ TypTextual ocamltype ]

let semvtypes = function
  | Symbol.T tok ->
      semvtypetok tok
  | Symbol.N nt ->
      [ semvtype nt ]

(* -------------------------------------------------------------------------- *)

(* [destruct_token_def] constructs the definition of a function that
   deconstructs tokens. *)

let destruct_token_def name codomain bindsemv mkbranch =
  let branches =
    Terminal.fold_real (fun t branches ->
      let pat  = tokpat t (if bindsemv then PVar semv else PWildcard)
      and body = mkbranch t in
      branch pat body :: branches
    ) []
  in
  let body = EFun ([ PVar token ], EMatch (EVar token, branches))
  and ty = arrow TokenType.ttoken codomain in
  def name (annotate body ty)

(* -------------------------------------------------------------------------- *)

end (* Make *)
