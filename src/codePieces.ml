(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module defines many internal naming conventions for use by the
   two code generators, [CodeBackend] and [TableBackend]. It also offers
   a few code generation facilities. *)

open IL
open ILConstruction
open MiddleAPI

module Make (G : GRAMMAR) (X : sig

  (**[exn_carries_state] determines whether the exception [Error] should carry
     an argument, which is a state number. This is the state where a syntax
     error is detected. This feature is supported only by the code back-end. *)
  val exn_carries_state: bool

  (**If [mode] is [`DefineTokenType] then the types [token] and ['a terminal]
     are defined as algebraic data types. If it is [`UseExternalTokenType "M"]
     then they are defined as abbreviations for pre-existing types [M.token]
     and ['a M.Interpreter.terminal]. *)
  val mode: [ `DefineTokenType | `UseExternalTokenType of string]

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

(* -------------------------------------------------------------------------- *)
(* Here is a bunch of naming conventions. Our names are chosen to minimize
   the likelihood that a name in a semantic action is captured. In other
   words, all global definitions as well as the parameters to [reduce]
   are given far-fetched names, unless [--no-prefix] was specified. Note
   that the prefix must begin with '_'. This allows avoiding warnings
   about unused variables with ocaml 3.09 and later. *)

let prefix name =
  if X.noprefix then
    name
  else
    (* This prefix must begin with an underscore. *)
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

(* The type variable associated with a nonterminal [nt]. *)

let ntvar nt =
  Infer.ntvar (Nonterminal.print true nt)

(* A variable used to hold a semantic value. *)

let semv =
  "_v"

(* A variable used to hold a stack. *)

let stack =
  prefix "stack"

(* A variable used to hold a state. *)

let state =
  prefix "s"

(* A variable used to hold a token. *)

let token =
  "_tok"

(* Variables used to hold start and end positions. Do not change these
   names! They are chosen to coincide with the $startpos and $endpos
   keywords, which the lexer rewrites to _startpos and _endpos, so
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

(* [semvtypent nt] is the type of the semantic value associated
   with nonterminal [nt]. *)

let semvtypent nt =
  match Nonterminal.ocamltype nt with
  | None ->

      (* [nt] has unknown type. If we have run [Infer], then this
         can't happen. However, running type inference is only an
         option, so we still have to deal with that case. *)

      ntvar nt

  | Some ocamltype ->

      (* [nt] has known type. *)

      TypTextual ocamltype

(* [semvtypetok tok] is the type of the semantic value associated with
   token [tok]. There is no such type if the token does not have a
   semantic value. *)

let semvtypetok tok =
  match Terminal.ocamltype tok with
  | None ->

      (* Token has unit type and is omitted in stack cell,
         unless [--represent-values] has been passed. *)

      if X.represent_values then [ tunit ] else []

  | Some ocamltype ->

      (* Token has known type. *)

      [ TypTextual ocamltype ]

(* [semvtype symbol] is the type of the semantic value associated with
   [symbol]. *)

let semvtype = function
  | Symbol.T tok ->
      semvtypetok tok
  | Symbol.N nt ->
      [ semvtypent nt ]

(* -------------------------------------------------------------------------- *)

(* Patterns for tokens. *)

let tokendata =
  TokenType.tokendata

(* [tokpat tok pat] is a pattern that matches the token [tok] and binds
   its semantic value (if it has one) to the pattern [pat]. *)

let tokpat tok pat =
  let data = tokendata (Terminal.print tok) in
  PData (
    data,
    if Terminal.ocamltype tok = None then [] else [ pat ]
  )

(* [tok_bind_unit tok pat e] binds the pattern [pat] to the unit value
   in the expression [e] if the token [tok] has no semantic value.
   Otherwise, it returns just [e]. *)

let tok_bind_unit tok pat e =
  if Terminal.ocamltype tok = None then
    blet ([ (pat, EUnit) ], e)
  else
    e

(* [tokspat toks] is a pattern that matches any token in the set [toks],
   without binding its semantic value. *)

let tokspat toks =
  POr (
    TerminalSet.fold (fun tok pats ->
      tokpat tok PWildcard :: pats
    ) toks []
  )

(* [destructuretokendef name codomain bindsemv branch] generates the
   definition of a function that destructures tokens. [name] is the
   name of the function that is generated. [codomain] is its return
   type. [bindsemv] tells whether the variable [semv] should be
   bound. [branch] is applied to each (real) terminal and must
   produce code for each branch. *)

let destructuretokendef name codomain bindsemv branch = {
  valpat = PVar name;
  valval =
    annotate (
      EFun ([ PVar token ],
        EMatch (EVar token,
          Terminal.fold (fun tok branches ->
            if Terminal.special tok then
              branches
            else
              { branchpat = tokpat tok (if bindsemv then PVar semv else PWildcard);
                branchbody = branch tok } :: branches
          ) []
        )
      )
    )
    (arrow TokenType.ttoken codomain)
}

(* -------------------------------------------------------------------------- *)

(* A toplevel function [stop] raises the exception [Error]. *)

(* If --exn-carries-state is passed, the exception [Error] carries an integer
   parameter, a state number. *)

let stop =
  "_eRR"

let call_stop (s : int) =
  if X.exn_carries_state then
    EApp (EVar stop, [EIntConst s])
  else
    EApp (EVar stop, [EUnit])

let stopdef =
  let s = "_s" in
  let args = if X.exn_carries_state then [ EVar s ] else [] in
  {
    valpat = PVar stop;
    valval =
      EFun ([PVar s],
        ERaise (EData (Interface.excname, args))
      )
  }

(* -------------------------------------------------------------------------- *)

(* Define the internal sub-module [Basics], which contains the definitions
   of the exception [Error] and of the type [token]. Then, include this
   sub-module. This is used both in the code and table back-ends. *)

let basics =
  "MenhirBasics"
  (* 2017/01/20 This name must be an unlikely name, as it might
     otherwise hide a user-defined module by the same name. *)

let mbasics grammar = [

  (* The module [Basics]. *)
  SIModuleDef (basics, MStruct (

    (* The exception [Error]. *)
    SIExcDefs [ Interface.excdef ] ::

    (* The definition of [stop] must be placed at this particular point
       so as to avoid the risk of a name collision. *)
    SIValDefs (false, [ stopdef ]) ::

    (* The type [token]. *)
    interface_to_structure (
      TokenType.tokentypedef grammar
    )

  ));

  (* Include the above submodule. *)
  SIInclude (MVar basics);

]

(* -------------------------------------------------------------------------- *)

end (* Make *)
