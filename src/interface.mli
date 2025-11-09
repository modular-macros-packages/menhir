(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module constructs the interface (that is, the [.mli] file)
   of the generated parser. *)

open IL
open PlainSyntax

(**[excname] is the name of the exception [Error],
   which is used to report a syntax error. *)
val excname: string

(**[excndef] is the definition of the exception [Error]. *)
val excdef: excdef

(**[incremental] is the the name of the submodule that contains the
   entry points of the incremental API. *)
val incremental: string

(**[checkpoint] constructs an application of the parameterized type
   ['a checkpoint], which is defined in the submodule [Interpreter].
   This type exists in the incremental API only. *)
val checkpoint: typ -> typ

(**[write grammar filename ()] writes the interface of the generated
   parser to the [.mli] file named [filename]. *)
val write: grammar -> string -> unit -> unit
