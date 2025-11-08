(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

type filename =
  string

val compile_errors: filename -> unit
val compare_errors: filename -> filename -> unit
val merge_errors: filename -> filename -> unit
val update_errors: filename -> unit
val echo_errors: filename -> unit
val echo_errors_concrete: filename -> unit
