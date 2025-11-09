(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(** [Refine] computes the refined partition of a list of overlapping set: the
    smallest list of non-overlapping sets such that each set is a subset of one
    of the input set.

    This is useful when computing the set of transitions of a DFA when the
    alphabet is large: rather than representing a transition for each letter
    separately, we group them by subset having the same destination.
*)

module type DECOMPOSABLE = sig

  type t
  (** The abstract type representing sets that we want to refine. *)

  val is_empty : t -> bool
  (** [is_empty s] determines whether the set [s] is empty. *)

  val compare_minimum : t -> t -> int
  (** [compare_minimum s1 s2] compares the nonempty sets [s1] and [s2]
      based on their minimum elements. *)

  val big_union : t list -> t
  (** [big_union ss] computes the union of all sets in the list [ss]. *)

  val extract_unique_prefix : t -> t -> t * t
  (** [extract_unique_prefix s1 s2] requires [compare_minimum s1 s2 < 0],
      that is, [minimum s1 < minimum s2]. It splits [s1] in two disjoint
      subsets [head1] and [tail1] such that [head1] is exactly the subset
      of [s1] whose elements are less than [minimum s2]. Therefore, [head1]
      must be nonempty, whereas [tail1] may be empty. *)

  val extract_shared_prefix : t -> t -> t * (t * t)
  (** [extract_shared_prefix s1 s2] requires [compare_minimum s1 s2 = 0],
      that is, [minimum s1 = minimum s2]. It splits [s1] and [s2] into
      three subsets [head], [tail1], and [tail2], as follows:

      - [s1] is [head U tail1] and [s2] is [head U tail2].
        This implies that [head] is a subset of both [s1] and [s2].
      - An element in [head] is smaller than every element in [tail1]
        and [tail2].
      - [head] is maximal with respect to the previous two properties.

      In summary, [head] is the maximal shared prefix of the sets [s1]
      and [s2]. *)
end

(** The type of refined sets *)
module type S = sig

  type t
  (** Type of a set, like [DECOMPOSABLE.t] *)

  val partition : t list -> t list
  (* [partition xs] computes the coarsest partition [ys] that refines
     a list [xs] of nonempty sets.

     [ys] is a partition of [U xs] iff:

     - No two sets in the list [ys] overlap, that is,
       [forall y1, y2 ∈ ys, z1 ∩ z2 = ∅].
     - The lists [ys] and [xs] cover the same universe, that is,
       [U ys = U xs]

     [ys] refines [xs] iff:

     - Every element of [xs] can be decomposed as a union of elements of [ys],
       that is,
       [forall x ∈ xs, exists zs ⊆ ys, x = U zs].

     The list [ys] returned by [partition xs] is sorted by [compare_minimum]. *)

end

module Make (Set : DECOMPOSABLE) : S with type t := Set.t
