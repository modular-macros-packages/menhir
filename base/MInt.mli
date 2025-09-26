(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)



(**[postincrement r] increments [r] and returns its original value. *)
val postincrement: int ref -> int

(**[mkgensym()] returns a fresh generator of unique integers. *)
val mkgensym : unit -> (unit -> int)
