(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

include GSet.S with type element = int

val diff: t -> t -> t
(** [diff s1 s2] is the set difference of [s1] and [s2].
    In other words, this set contains the elements of [s1]
    that are not elements of [s2]. *)

val above: element -> t -> t
(** [above x s] is the set of the elements of [s] that are greater
    than [x]. *)

val find_first_opt: (element -> bool) -> t -> element option
(** [find_first f s] returns the lowest element [e] of [s] such that [f e],
    or [None] if no such element exists. *)

module View : sig
  type view =
    | N
    | C of int * AtomicBitSet.t * t
  val view : t -> view
end
