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

  (* Leftist heaps whose keys are sets. *)

  type 'v heap =
    | Leaf
    | Node of 'v heap * set * 'v * 'v heap * int

  let singleton k v =
    Node (Leaf, k, v, Leaf, 1)

  let rank t =
    match t with
    | Leaf -> 0
    | Node (_, _, _, _, r) -> r

  let rec merge t1 t2 =
    match t1, t2 with
    | Leaf, t
    | t, Leaf ->
        t
    | Node (l, k1, v1, r, _), Node (_, k2, _, _, _) ->
        if Set.compare_minimum k1 k2 > 0 then
          merge t2 t1
        else
          let merged = merge r t2 in (* always merge with right *)
          let lrank = rank l
          and rrank = rank merged in
          if lrank >= rrank
          then Node (l, k1, v1, merged, rrank + 1)
          else Node (merged, k1, v1, l, lrank + 1)
               (* left becomes right due to being shorter *)

  let insert k v t =
    merge (singleton k v) t

  type 'v pop =
    | Head of set * 'v * set * 'v * 'v heap
    | Tail of set * 'v
    | Done

  let pop2 (t : 'v heap) : 'v pop =
    match t with
    | Leaf ->
        Done
    | Node (Leaf, k, v, _, _) ->
        Tail (k, v)
    | Node (Node (ll, lk, lv, lr, _), k, v, Leaf, _) ->
        Head (k, v, lk, lv, merge ll lr)
    | Node (
        (Node (ll, lk, lv, lr, _) as l),
        k, v,
        (Node (rl, rk, rv, rr, _) as r),
        _
      ) ->
        if Set.compare_minimum lk rk <= 0
        then Head (k, v, lk, lv, merge (merge ll lr) r)
        else Head (k, v, rk, rv, merge (merge rl rr) l)

  type part =
    set * IntSet.t

  type parts =
    part list

  let[@inline] cons_if_nonempty s k parts : parts =
    if not (Set.is_empty s) then (s, k) :: parts else parts

  let[@inline] insert_if_nonempty s k heap : 'v heap =
    if not (Set.is_empty s) then insert s k heap else heap

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
      ) Leaf xs
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
