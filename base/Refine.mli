(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module implements a [partition] function, which, given a list [xs] of
   possibly-overlapping sets, computes the coarsest partition of the the set
   [U xs] that refines [xs]. In other words, it computes the coarsest family
   [ys] of non-overlapping sets such that each set in the list [xs] can be
   written as a disjoint union of members of [ys].

   This is useful when representing the transitions of a DFA whose alphabet is
   large. Rather than representing one transition per symbol, we group the
   symbols into classes whose elements are never distinguished. *)

(**The operations required by [Make] form a fragment of the signature
   [Bitsets.API.SET]. *)
module Make (Set : sig
  type t
  val is_empty : t -> bool
  val compare_minimum : t -> t -> int
  val big_union : t list -> t
  val extract_unique_prefix : t -> t -> t * t
  val extract_shared_prefix : t -> t -> t * (t * t)
end) : sig

  open Set

  (**If [xs] is a list of nonempty sets then [partition xs] is the coarsest
     partition of the set [U xs] that refines [xs].

     The list [ys] is a partition of the set [U xs] iff:

     - The sets in the list [ys] are pairwise disjoint;
     - The union of the sets in the list [ys] is [U xs].

     [ys] refines [xs] iff:

     - Every set in the list [xs] is a union of elements of [ys].

     The list [partition xs] is sorted by [compare_minimum]. *)
  val partition : t list -> t list

end
