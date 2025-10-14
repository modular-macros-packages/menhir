(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This modules defines the syntax of sorts. *)

(**The syntax of sorts, on paper, would be [sort ::= (sort, ..., sort) -> *].
   The arity of a sort [GArrow sorts] is the length of the list [sorts]. This
   arity can be zero. *)
type sort =
  | GArrow of sort list

(**[star] is the base sort. It is the sort of arity zero, [GArrow []]. *)
val star: sort

(**The domain of the sort [GArrow sorts] is the list [sorts]. *)
val domain: sort -> sort list
