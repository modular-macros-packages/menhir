(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**These functions compute the free names in several syntactic categories.  *)

open Syntax

val branches : parameterized_branch list -> StringSet.t
val rule : parameterized_rule -> StringSet.t
