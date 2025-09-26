(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* Tabulating a function using an internal array. [tabulate n f]
   returns a function that is extensionally equal to [f], but relies
   on an internal array. Arguments to [f] are of type [int] and are
   supposed to lie in the range [0..n). *)

val tabulate: int -> (int -> 'a) -> (int -> 'a)

(* [with_buffer n f] creates a fresh buffer of size [n], passes it to [f],
   and returns the final content of the buffer. *)

val with_buffer: int -> (Buffer.t -> unit) -> string

type 'a iter = ('a -> unit) -> unit

(**[separated_list_to_string printer sep xs] converts [xs] into a string by
   applying [printer] to each element and by inserting [sep] as a separator
   between two successive elements. *)
val separated_list_to_string:  ('a -> string) -> string -> 'a list -> string
val separated_iter_to_string:  ('a -> string) -> string -> 'a iter -> string

(**[preceded_list_to_string printer del xs] converts the list [xs] into a
   string by applying [printer] to each element and by preceding each element
   with the delimiter [del]. *)
val preceded_list_to_string: ('a -> string) -> string -> 'a list -> string
val preceded_iter_to_string: ('a -> string) -> string -> 'a iter -> string

(* Turning an implicit list, stored using pointers through a hash
   table, into an explicit list. The head of the implicit list is
   not included in the explicit list. *)

val materialize: ('a, 'a option) Hashtbl.t -> 'a -> 'a list

(* [normalize s] returns a copy of [s] where parentheses and commas
   are replaced with underscores. *)

val normalize: string -> string

(* [filter_map f l] returns the list of [y]s such that [f x = Some y] where [x]
   is in [l], preserving the order of elements of [l]. *)
val filter_map : ('a -> 'b option) -> 'a list -> 'b list

(* [new_encode_decode capacity] creates a new service for assigning unique
   integer codes to strings. [capacity] is the initial capacity of the
   internal hash table. [new_encode_decode] returns a triple [encode, decode,
   verbose], where [encode] and [decode] translate between strings and unique
   integer codes and [verbose] prints statistics about the use of the service
   so far. *)
val new_encode_decode: int -> (string -> int) * (int -> string) * (unit -> unit)

(* If [preferable] is a partial order on elements, then [best preferable xs]
   returns the best (least) element of [xs], if there is one. Its complexity
   is quadratic. *)

val best: ('a -> 'a -> bool) -> 'a list -> 'a option

(* Assuming that the list [xs] is sorted with respect to the ordering [cmp],
   [levels cmp xs] is the list of levels of [xs], where a level is a maximal
   run of adjacent equal elements. Every level is a nonempty list. *)

val levels: ('a -> 'a -> int) -> 'a list -> 'a list list

(* Assuming that the list [xs] is sorted with respect to the ordering [cmp],
   [dup cmp xs] returns a duplicate element of the list [xs], if one exists. *)

val dup: ('a -> 'a -> int) -> 'a list -> 'a option

(* [once x y] produces a function [f] which produces [x] the first time it
   is called and produces [y] forever thereafter. *)

val once: 'a -> 'a -> (unit -> 'a)

(* A nice way of printing [n] in English, for concrete values of [n]. *)

val count: int -> string

(* A nice way of printing "nth" in English, for concrete values of [n]. *)

val nth: int -> string

(* [padded_index n i] produces a padded string representation of the index
   [i], which must lie in the semi-open interval [0, n). It is defined in such
   a way that all indices are mapped to strings of equal length. This ensures
   that alphabetical ordering coincides with numeric ordering. *)

val padded_index: int -> int -> string

(**[longest_run c s] computes the length of the longest run of consecutive
   characters equal to [c] in the string [s]. *)
val longest_run: char -> string -> int

(**[unquote s] assumes that the string [s] begins and ends with a quote
   character ['"']. It removes these quotes and returns the remainder
   of the string, in which escape sequences have been decoded. *)
val unquote: string -> string
