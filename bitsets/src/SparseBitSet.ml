(******************************************************************************)
(*                                                                            *)
(*                                  Bitsets                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

module W =
  WordBitSet

(* An offset is a nonnegative multiple of [W.bound]. *)

type offset =
  int (* a multiple of [W.bound] *)

let[@inline] check_offset (o : offset) =
  assert (0 <= o);
  assert (o mod W.bound = 0)

let[@inline] compare_offsets (o1 : offset) (o2 : offset) : int =
  compare o1 o2

(* A sparse bit set is a linked list of pairs of an offset [o] and a nonempty
   bit set [w]. The list is sorted by increasing order of offsets. *)

type elt =
  int

type word =
  W.t

type t =
  | N
  | C of offset * word * t

let rec check1 o s =
  match s with
  | N -> ()
  | C (o', w, s) ->
      check_offset o';
      assert (o < o');
      assert (not (W.is_empty w));
      check1 o' s

let check s =
  match s with
  | N -> ()
  | C (o, w, s) ->
      check_offset o;
      assert (not (W.is_empty w));
      check1 o s

(* -------------------------------------------------------------------------- *)

(* Construction. *)

let empty =
  N

let[@inline] construct o w s =
  if W.is_empty w then s else C (o, w, s)

let rec add1 base i s =
  check_offset base;
  match s with
  | N ->
      (* Insert at end. *)
      C (base, W.singleton i, empty)
  | C (o, w, qs) ->
      if base < o then
        (* Insert in front. *)
        C (base, W.singleton i, s)
      else if base = o then
        (* Found appropriate cell, update bit field. *)
        let w' = W.add i w in
        if W.equal w' w then s else C (o, w', qs)
      else
        (* Not there yet, continue. *)
        let qs' = add1 base i qs in
        if qs == qs' then s else C (o, w, qs')

let[@inline] add x s =
  let i = x mod W.bound in
  let base = x - i in
  add1 base i s

let[@inline] singleton x =
  let i = x mod W.bound in
  let base = x - i in
  (* This is [add1 base i empty], specialized. *)
  C (base, W.singleton i, empty)

let rec remove1 base i s =
  match s with
  | N ->
      empty
  | C (o, w, qs) ->
      if base < o then
        s
      else if base = o then
        (* Found appropriate cell, update bit field. *)
        let ss' = W.remove i w in
        if W.is_empty ss' then
          qs
        else if W.equal ss' w then s else C (o, ss', qs)
      else
        (* Not there yet, continue. *)
        let qs' = remove1 base i qs in
        if qs == qs' then s else C (o, w, qs')

let[@inline] remove x s =
  let i = x mod W.bound in
  let base = x - i in
  remove1 base i s

let rec union s1 s2 =
  match s1, s2 with
  | N, s
  | s, N ->
      s
  | C (o1, w1, qs1), C (o2, w2, qs2) ->
      if o1 < o2 then
        C (o1, w1, union qs1 s2)
      else if o1 > o2 then
        let s = union s1 qs2 in
        if s == qs2 then s2 else C (o2, w2, s)
      else
        let ss = W.union w1 w2 in
        let s = union qs1 qs2 in
        if W.equal ss w2 && s == qs2 then s2 else C (o1, ss, s)

(* [inter] attempts to preserve sharing between its first argument and its
   result. This is an arbitrary decision; furthermore, it is not mandatory,
   as the specification of [inter] does not guarantee this. *)

let rec inter s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
      empty
  | C (o1, w1, qs1), C (o2, w2, qs2) ->
      if o1 < o2 then
        inter qs1 s2
      else if o1 > o2 then
        inter s1 qs2
      else
        let ss = W.inter w1 w2 in
        let s = inter qs1 qs2 in
        if W.is_empty ss then s else
        if W.equal ss w1 && s == qs1 then s1 else C (o1, ss, s)

let rec diff s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
      s1
  | C (o1, w1, qs1), C (o2, w2, qs2) ->
      if o1 < o2 then
        let qs1' = diff qs1 s2 in
        if qs1' == qs1 then s1 else C (o1, w1, qs1')
      else if o1 > o2 then
        diff s1 qs2
      else
        let ss = W.diff w1 w2 in
        if W.is_empty ss then
          diff qs1 qs2
        else
          let qs1' = diff qs1 qs2 in
          if W.equal ss w1 && qs1' == qs1 then s1 else C (o1, ss, qs1')

let rec above1 base i s =
  match s with
  | N ->
      empty
  | C (o, w, qs) ->
      if base < o then
        (* Stop now. *)
        s
      else if base = o then
        (* Found appropriate cell, split bit field. *)
        let ss' = W.above i w in
        if W.is_empty ss' then
          qs
        else if W.equal w ss' then
          s
        else
          C (o, ss', qs)
      else
        (* Not there yet, continue. *)
        above1 base i qs

let[@inline] above x s =
  let i = x mod W.bound in
  let base = x - i in
  above1 base i s

(* -------------------------------------------------------------------------- *)

(* Cardinality. *)

let[@inline] is_empty s =
  match s with N -> true | C _ -> false

let is_singleton s =
  match s with
  | C (_, ss, N) ->
      W.is_singleton ss
  | C (_, _, C _)
  | N ->
      false

let rec cardinal accu s =
  match s with
  | C (_, w, qs) ->
      let accu = accu + W.cardinal w in
      cardinal accu qs
  | N ->
      accu

let[@inline] cardinal s =
  cardinal 0 s

(* -------------------------------------------------------------------------- *)

(* Tests. *)

let rec mem1 base i s =
  match s with
  | N ->
      false
  | C (o, w, qs) ->
      if base < o then
        false
      else if base = o then
        W.mem i w
      else
        mem1 base i qs

let[@inline] mem x s =
  let i = x mod W.bound in
  let base = x - i in
  mem1 base i s

let rec equal s1 s2 =
  s1 == s2 ||
  match s1, s2 with
  | N, N ->
      true
  | C _, N
  | N, C _ ->
      false
  | C (o1, w1, qs1), C (o2, w2, qs2) ->
      o1 = o2 && W.equal w1 w2 && equal qs1 qs2

let rec compare s1 s2 =
  if s1 == s2 then 0 else
  match s1, s2 with
  | N  , N ->  0
  | C _, N -> +1
  | N, C _ -> -1
  | C (o1, w1, qs1), C (o2, w2, qs2) ->
      let c = compare_offsets o1 o2 in if c <> 0 then c else
      let c = W.compare w1 w2 in if c <> 0 then c else
      let c = compare qs1 qs2 in c

let rec disjoint s1 s2 =
  match s1, s2 with
  | N, _
  | _, N ->
      true
  | C (o1, w1, qs1), C (o2, w2, qs2) ->
      if o1 = o2 then
        W.disjoint w1 w2 && disjoint qs1 qs2
      else if o1 < o2 then
        disjoint qs1 s2
      else
        disjoint s1 qs2

let rec subset s1 s2 =
  match s1, s2 with
  | N, _ ->
      true
  | _, N ->
      false
  | C (o1, w1, qs1), C (o2, w2, qs2) ->
      if o1 < o2 then
        false
      else if o1 = o2 then
        W.subset w1 w2 && subset qs1 qs2
      else
        subset s1 qs2

let rec quick_subset1 o1 w1 s2 =
  match s2 with
  | N ->
      false
  | C (o2, w2, qs2) ->
      if o1 = o2 then
        W.quick_subset w1 w2
      else
        o1 > o2 && quick_subset1 o1 w1 qs2

let[@inline] quick_subset s1 s2 =
  match s1 with
  | N ->
      false
  | C (o1, w1, _) ->
      (* [w1] must be not empty. Therefore it suffices to test whether the
         elements represented by [o1] and [w1] appear in the set [s2]. *)
      quick_subset1 o1 w1 s2

(* -------------------------------------------------------------------------- *)

(* Extraction. *)

let[@inline] minimum s =
  match s with
  | N ->
      raise Not_found
  | C (o, w, _) ->
      o + W.minimum w

let rec maximum1 o w s =
  match s with
  | N ->
      o + W.maximum w
  | C (o, w, s) ->
      maximum1 o w s

let maximum s =
  match s with
  | N ->
      raise Not_found
  | C (o, w, s) ->
      maximum1 o w s

let choose =
  minimum

(* -------------------------------------------------------------------------- *)

(* Iteration. *)

let rec iter yield s =
  match s with
  | N ->
      ()
  | C (o, w, qs) ->
      W.iter_delta o yield w;
      iter yield qs

let rec fold yield s accu =
  match s with
  | N ->
      accu
  | C (o, w, qs) ->
      let accu = W.fold_delta o yield w accu in
      fold yield qs accu

let[@inline] elements s =
  fold (fun tl hd -> tl :: hd) s []

exception Found of elt

let find_first_opt p s =
  try
    iter (fun x -> if p x then raise (Found x)) s;
    None
  with Found x ->
    Some x

(* -------------------------------------------------------------------------- *)

(* Decomposition. *)

let compare_minimum s1 s2 =
  match s1, s2 with
  | N, N ->  0
  | N, _ -> -1
  | _, N -> +1
  | C (o1, w1, _), C (o2, w2, _) ->
      let c = Int.compare o1 o2 in if c <> 0 then c else
      W.compare_minimum w1 w2

let[@inline] sorted_union ss =
  (* The list [ss] is sorted. By starting from its right end, we repeatedly
     prepend elements to the accumulator. This makes the complexity of this
     algorithm linear in the length of the list [ss]. Starting from the left
     end would make it quadratic. *)
  List.fold_right union ss empty

let rec extract_unique_prefix1 o2 w2 s1 =
  match s1 with
  | N ->
      empty, empty
  | C (o1, w1, qs1) ->
      if o1 < o2 then
        let head1, tail1 = extract_unique_prefix1 o2 w2 qs1 in
        (if qs1 == head1 then s1 else C (o1, w1, head1)),
        tail1
      else if o1 > o2 || W.equal w1 w2 then
        empty, s1
      else
        let w1a, w1b = W.extract_unique_prefix w1 w2 in
        if W.is_empty w1a then
          empty, s1
        else
          C (o1, w1a, empty),
          if W.is_empty w1b then qs1 else
          if w1 == w1b then s1 else C (o1, w1b, qs1)

let[@inline] extract_unique_prefix s1 s2 =
  assert (not (is_empty s2));
  match s1, s2 with
  | N, _
  | _, N ->
      empty, empty
  | _, C (o2, w2, _) ->
      extract_unique_prefix1 o2 w2 s1

let rec extract_shared_prefix s1 s2 =
  match s1, s2 with
  | C (o1, w1, qs1), C (o2, w2, qs2) when o1 = o2 ->
      if W.equal w1 w2 then
        let head, tails = extract_shared_prefix qs1 qs2 in
        C (o1, w1, head), tails
      else
        let head, (w1, w2) = W.extract_shared_prefix w1 w2 in
        let head = construct o1 head empty in
        let qs1 = construct o1 w1 qs1 in
        let qs2 = construct o2 w2 qs2 in
        head, (qs1, qs2)
  | _, _ ->
      empty, (s1, s2)

(* -------------------------------------------------------------------------- *)

(* View. *)

type view = t =
  | N
  | C of offset * word * t

let[@inline] view x = x
