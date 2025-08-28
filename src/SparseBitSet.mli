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
(** Set difference: [diff s1 s2] contains the elements of [s1]
    that are not in [s2]. *)

val take_after: element -> t -> t
(** [take_after x s] contains the elements of [s] that are strictly
    greater than [x]. *)

val find_first_opt: (element -> bool) -> t -> element option
(** [find_first f s] returns the lowest element [e] of [s] such that [f e],
    or [None] if no such element exists. *)

module View : sig
  type view =
    | N
    | C of int * AtomicBitSet.t * t
  val view : t -> view
end
