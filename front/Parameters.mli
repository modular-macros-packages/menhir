(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Syntax

val apply : symbol located -> parameters -> parameter
val unapp : parameter -> symbol located * parameters
val unvar : parameter -> symbol located
val map :
  (symbol located -> symbol located) ->
  parameter -> parameter
val identifiers :
  parameters -> range StringMap.t
val occurs : symbol -> parameter -> bool
val occurs_shallow : symbol -> parameter -> bool
val occurs_deep : symbol -> parameter -> bool
type t = parameter
val equal : t -> t -> bool
val hash : t -> int
val position : parameter -> range
val locate : parameter -> parameter located
val print : bool -> parameter -> string
