(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module defines the interface of the generated parser. *)

open IL
open PlainSyntax

(* This is the [Error] exception. *)

val excname: string
val excdef: excdef

(* The type ['a checkpoint], defined in the interpreter sub-module. *)

val checkpoint: typ -> typ

(* The name of the sub-module that contains the incremental entry points. *)

val incremental: string

(* This writes the interface of the generated parser to the [.mli] file. *)

val write: grammar -> unit -> unit
