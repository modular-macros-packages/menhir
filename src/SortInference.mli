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
open Sort

(**A sort assignment is a map of (terminal and nonterminal) symbols to sorts. *)
type sorts =
  sort StringMap.t

(**[infer g] performs sort inference for the grammar [g]. If the grammar is
   well-sorted, it returns a sort assignment. Otherwise, it fails. *)
val infer: grammar -> sorts
