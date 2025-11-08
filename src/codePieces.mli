(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module defines many internal naming conventions for use by the
   two code generators, [CodeBackend] and [TableBackend]. It also
   offers a few code generation facilities. *)

open IL
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

end) : sig
open G

(* -------------------------------------------------------------------------- *)

(* These functions are used to generate names in menhir's namespace. *)
val prefix: string -> string
val dataprefix: string -> string
  (* only stacklang *)
val tvprefix: string -> string
  (* only stacklang *)

(* ------------------------------------------------------------------------ *)

(* Naming conventions. *)

(* A variable used to hold a semantic value. *)

val semv : string

(* A variable used to hold a stack. *)

val stack: string

(* A variable used to hold a state. *)

val state: string

(* A variable used to hold a token. *)

val token: string
  (* only stacklang *)

(* Variables used to hold start and end positions. *)

val beforeendp: string
val startp: string
val endp: string

val startpos: string array -> int -> string
  (* only stacklang *)
val endpos: string array -> int -> string
  (* only stacklang *)

(* ------------------------------------------------------------------------ *)

(* Types for semantic values. *)

(* [semvtypent nt] is the type of the semantic value associated
   with nonterminal [nt]. *)

val semvtypent : Nonterminal.t -> typ

(* [semvtype symbol] is the type of the semantic value associated with
   [symbol]. *)

val semvtype : Symbol.t -> typ list

(* ------------------------------------------------------------------------ *)

(* Patterns for tokens. *)

(* [destructuretokendef name codomain bindsemv branch] generates the
   definition of a function that destructure tokens. [name] is the
   name of the function that is generated. [codomain] is its return
   type. [bindsemv] tells whether the variable [semv] should be
   bound. [branch] is applied to each (real) terminal and must
   produce code for each branch. *)

val destructuretokendef: string -> typ -> bool -> (Terminal.t -> expr) -> valdef

(* ------------------------------------------------------------------------ *)

(* A toplevel function [stop] raises the exception [Error]. *)

(* If --exn-carries-state is passed, the exception [Error] carries an integer
   parameter, a state number. This state number must be passed as an argument
   to [call_stop]. In the absence of --exn-carries-state, [call_stop] ignores
   its argument. *)

val call_stop: int -> expr
  (* only stacklang *)

(* ------------------------------------------------------------------------ *)

(* The structure items [mbasics grammar] define and include the internal
   sub-module [Basics], which contains the definitions of the exception
   [Error] and of the type [token]. Then, they define the global variable
   mentioned above, which holds the exception [Error]. *)

val basics: string

val mbasics: PlainSyntax.grammar -> structure

end
