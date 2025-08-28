(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open MenhirLib

(* [compress ~hole t] turns the two-dimensional table [t] into a compressed
   table. The parameter [hole] is the value of empty cells in the table and is
   used to fill holes in the data array. *)
val compress : hole:int -> int array array -> int RowDisplacement.table
