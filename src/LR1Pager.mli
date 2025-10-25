(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module implements the construction of an LR(1) automaton following a
   version of Pager's algorithm. *)

open MiddleAPI

module Make (Lr0 : LR0) : LR1_ALGORITHM with module Lr0 = Lr0
