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
open Grammar

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

(* [tokpat tok pat] is a pattern that matches the token [tok] and binds
   its semantic value (if it has one) to the pattern [pat]. *)

val tokpat: Terminal.t -> pattern -> pattern
  (* only stacklang *)

(* [tokspat toks] is a pattern that matches any token in the set [toks],
   without binding its semantic value. *)

val tokspat: TerminalSet.t -> pattern
  (* only stacklang *)

(* [tok_bind_unit tok pat e] binds the pattern [pat] to the unit value
   in the expression [e] if the token [tok] has no semantic value.
   Otherwise, it returns just [e]. *)

val tok_bind_unit: Terminal.t -> pattern -> expr -> expr
  (* only stacklang *)

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
