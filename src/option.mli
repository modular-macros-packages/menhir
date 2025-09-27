(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

val map: ('a -> 'b) -> 'a option -> 'b option
val iter: ('a -> unit) -> 'a option -> unit
val fold: ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
val get: 'a option -> 'a
val equal: ('a -> 'b -> bool) -> 'a option -> 'b option -> bool
val compare: ('a -> 'b -> int) -> 'a option -> 'b option -> int
val hash: ('a -> int) -> 'a option -> int
val value: 'a option -> default:'a -> 'a
val split: ('a * 'b) option -> 'a option * 'b option
val sub: ('a -> 'b -> bool) -> 'a option -> 'b option -> bool (* None <= Some *)
