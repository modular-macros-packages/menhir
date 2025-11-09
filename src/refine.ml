(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

module IntSet =
  Bitsets.SparseBitSet

module Make (Set : sig
  type t
  val is_empty : t -> bool
  val compare_minimum : t -> t -> int
  val big_union : t list -> t
  val extract_unique_prefix : t -> t -> t * t
  val extract_shared_prefix : t -> t -> t * (t * t)
end) = struct

  type set =
    Set.t

  type sets =
    set list

  module H =
    LeftistHeap.Make(struct
      type t = Set.t
      let compare = Set.compare_minimum
    end)
  open H

  type part =
    set * IntSet.t

  type parts =
    part list

  let[@inline] cons_if_nonempty s k parts : parts =
    if not (Set.is_empty s) then (s, k) :: parts else parts

  let[@inline] insert_if_nonempty s k heap : 'v heap =
    if not (Set.is_empty s) then H.insert s k heap else heap

  let rec aux (parts : parts) (heap : IntSet.t heap) : parts =
    match pop2 heap with
    | Head (s1, k1, s2, k2, heap) ->
        let sp, s1 = Set.extract_unique_prefix s1 s2 in
        let sc, (s1, s2) = Set.extract_shared_prefix s1 s2 in
        let parts = cons_if_nonempty sp k1 parts in
        let heap =
          (* [insert_if_nonempty sc (IntSet.union k1 k2)] *)
          if not (Set.is_empty sc) then insert sc (IntSet.union k1 k2) heap
          else heap
        in
        let heap = insert_if_nonempty s1 k1 heap in
        let heap = insert_if_nonempty s2 k2 heap in
        aux parts heap
    | Tail (k, v) ->
        (k, v) :: parts
    | Done ->
        parts

  let compute_parts (xs : sets) : parts =
    let c = ref 0 in
    let heap =
      List.fold_left (fun h s ->
        let i = !c in
        c := i + 1;
        insert s (IntSet.singleton i) h
      ) empty xs
    in
    aux [] heap

  let compare_parts (_, k1) (_, k2) =
    IntSet.compare k1 k2

  let rec merge accu ss v parts : sets =
    match parts with
    | [] ->
        Set.big_union ss :: accu
    | (s, v') :: parts ->
        if IntSet.equal v v' then
          merge accu (s :: ss) v parts
        else
          merge (Set.big_union ss :: accu) [s] v' parts

  let union_parts (parts : parts) : sets =
    match List.sort compare_parts parts with
    | [] ->
        []
    | (s1, k1) :: parts ->
        merge [] [s1] k1 parts

  let partition xs =
    union_parts (compute_parts xs)

end
