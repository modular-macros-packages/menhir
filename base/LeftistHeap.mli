(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a priority queue, represented as a leftist heap. *)

module Make (Key : sig
  type t
  val compare: t -> t -> int
end) : sig

  (**A key. *)
  type key =
    Key.t

  (**An immutable priority queue. *)
  type 'a heap

  type 'a t =
    'a heap

  (**[empty] is the empty queue. *)
  val empty : 'a t

  (**[singleton k v] is a singleton queue containing the key-value
     pair [(k, v)]. *)
  val singleton : key -> 'a -> 'a t

  (**[insert k v q] inserts the key-value pair [(k, v)] into the queue
     [q]. The result is a new queue. *)
  val insert : key -> 'a -> 'a t -> 'a t

  (**[merge q1 q2] merges the queues [q1] and [q2]. The result is a
     new queue. *)
  val merge : 'a t -> 'a t -> 'a t

  type 'a pop2 =
    | Head of key * 'a * key * 'a * 'a t
    | Tail of key * 'a
    | Done

  (**[pop2 q] extracts at most two elements out of the queue [q].
     The element or elements that are extracted have minimal keys
     according to the ordering [Key.compare]. *)
  val pop2 : 'a t -> 'a pop2

  (**/**)

  val check: 'a t -> unit

end
