(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(** This module is an extension of Stdlib.List *)

include module type of List

(**If [xs] is a singleton list then [single xs] is its single element. *)
val single: 'a list -> 'a

(**An alias for [List.init], when it exists. *)
val init : int -> (int -> 'a) -> 'a list

(**[ifn c xs] constructs the list [if c then xs else []]. Be careful: the
   expression [xs] is evaluated even if the condition [c] is false. *)
val ifn : bool -> 'a list -> 'a list

(**[ifnlazy c xs] constructs the list [if c then xs() else []]. *)
val ifnlazy : bool -> (unit -> 'a list) -> 'a list

(**[if1 c x] constructs the list [if c then [x] else []]. Be careful: the
   expression [x] is evaluated even if the condition [c] is false. *)
val if1 : bool -> 'a -> 'a list

(**[cons_if c x xs] constructs the list [if c then x :: xs else xs]. Be
   careful: the expression [x] is evaluated even if the condition [c] is
   false. *)
val cons_if : bool -> 'a -> 'a list -> 'a list

(**The sum of a list of integers. *)
val sum : int list -> int

(**[drop k xs] is the list [xs] deprived of its first [k] elements. *)
val drop : int -> 'a list -> 'a list

(**[take k xs] is the list of the first [k] elements of the list [xs]. *)
val take : int -> 'a list -> 'a list

(**Given a [leq_join] function on elements, [leq_join] constructs a [leq_join]
   function on lists. The two lists must have the same length. The
   specification of a [leq_join] is defined by the signature
   [Fix.MINIMAL_SEMI_LATTICE]. *)
val leq_join : ('a -> 'b -> 'b) -> 'a list -> 'b list -> 'b list

(**[group cmp xs] first sorts the list [xs] with respect to the ordering
   [cmp], then returns a list of groups, where each group is a maximal run of
   adjacent equivalent elements. Every group is a nonempty list. *)
val group: ('a -> 'a -> int) -> 'a list -> 'a list list

(**[find_map f xs] applies [f] to the elements of [xs] in order and returns
   the first result of the form [Some y], if there is one. If there is none,
   it returns [None]. *)
val find_map : ('a -> 'b option) -> 'a list -> 'b option

(**[filter_map f xs] returns the list of the elements [y] such that
   [f x = Some y], where [x] ranges over the list [xs]. *)
val filter_map : ('a -> 'b option) -> 'a list -> 'b list

(**[partition_map] splits the list [xs] in two sublists; each element [x]
   is placed in the left-hand or right-hand result list according to the
   result of the function call [f x]. *)
val partition_map :
  ('a -> [< `L of 'l | `R of 'r ]) -> 'a list -> 'l list * 'r list

(**[equal eq xs ys] tests whether the lists [xs] and [ys] are equal. The
   elements of the lists are compared using the function [eq]. *)
val equal : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

(**[compare cmp xs ys] compares the lists [xs] and [ys] according to the
   lexicographic ordering. The elements of the lists are compared using the
   total ordering function [cmp]. *)
val compare : ('a -> 'b -> int) -> 'a list -> 'b list -> int

(**If [h] is a hash function for elements
   then [hash h] is a hash function for lists. *)
val hash: ('a -> int) -> 'a list -> int

(**[extract p xs] searches the list [xs] for an element [x] such that [p x] is
   true. It returns a pair of this element (if it exists) and a list of the
   remaining elements. *)
val extract : ('a -> bool) -> 'a list -> 'a option * 'a list

(**[uniq cmp xs] assumes that the list [xs] is sorted according to the
   ordering [cmp] and returns the list [xs] deprived of any duplicate
   elements. *)
val uniq: ('a -> 'a -> int) -> 'a list -> 'a list

(**[merge_uniq cmp l1 l2] merges two sorted lists (without duplicate
   elements) and produces a sorted list (without duplicate elements). *)
val merge_uniq : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list

(**[reduce e f l] reduces the list [xs] using the associative binary
   operation [f], whose unit (neutral element) is [e].

   The reduction is performed in a balanced fashion.
   For example, [reduce f [a; b; c; d]] is [f (f a b) (f c d)]. *)
val reduce : 'a -> ('a -> 'a -> 'a) -> 'a list -> 'a
