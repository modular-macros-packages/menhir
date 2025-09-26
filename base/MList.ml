(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

include List

let single xs =
  match xs with
  | [ x ] ->
      x
  | _ ->
      assert false

let[@tail_mod_cons] rec init_aux i n f =
  if n <= i then
    []
  else
    let r = f i in
    r :: init_aux (i+1) n f

let[@inline] init n f =
  assert (0 <= n);
  init_aux 0 n f

let[@inline] ifn c xs =
  if c then xs else []

let[@inline] if1 c x =
  if c then [x] else []

let[@inline] cons_if c x xs =
  if c then x :: xs else xs

let[@inline] ifnlazy c xs =
  if c then xs() else []

let[@inline] sum li =
  fold_left (+) 0 li

let rec drop k xs =
  match k, xs with
  | 0, _
  | _, [] ->
      xs
  | _, _ :: xs ->
      drop (k - 1) xs

let[@tail_mod_cons] rec take k xs =
  match k, xs with
  | 0, _
  | _, [] ->
      []
  | _, x :: xs ->
      x :: take (k - 1) xs

let leq_join leq_join xs1 xs2 =
  try
    let xs = List.map2 leq_join xs1 xs2 in
    if for_all2 (==) xs2 xs then xs2 else xs
  with Invalid_argument _ ->
    (* The lists have different lengths. *)
    assert false

let rec group1 cmp x1 xs =
  match xs with
  | [] ->
      [x1], []
  | x2 :: xs ->
      let ys1, yss = group1 cmp x2 xs in
      if cmp x1 x2 = 0 then
        x1 :: ys1, yss
      else
        [x1], ys1 :: yss

let group cmp xs =
  match List.sort cmp xs with
  | [] ->
      []
  | x1 :: xs ->
      let ys1, yss = group1 cmp x1 xs in
      ys1 :: yss

let group_by
  ~(compare:'a -> 'a -> int)
  ~(group:'a -> 'a list -> 's)
  (xs : 'a list) : 's list
=
  match List.sort compare xs with
  | [] ->
      []
  | x :: xs ->
      (* [summaries] is the list of the group summaries obtained so far. *)
      (* [xs] and [x] form the current group, which is nonempty. *)
      (* [ys] is the list of elements that remain to be scanned. *)
      let rec loop summaries xs x ys =
        match ys with
        | [] ->
            group x xs :: summaries
        | y :: ys ->
            if compare x y = 0
            then loop summaries (y :: xs) x ys
            else loop (group x xs :: summaries) [] y ys
      in
      loop [] [] x xs

let rec find_map f = function
  | [] ->
      None
  | x :: xs ->
      match f x with
      | Some _ as o -> o
      | None -> find_map f xs

let rec filter_map_rev f accu = function
  | [] ->
      accu
  | x :: xs ->
      let accu = match f x with None -> accu | Some y -> y :: accu in
      filter_map_rev f accu xs

let filter_map f xs =
  filter_map_rev f [] xs
  |> List.rev

let rec partition_map f = function
  | [] ->
      [], []
  | x :: xs ->
      let y = f x in
      let ls, rs = partition_map f xs in
      match y with
      | `L l -> (l :: ls, rs)
      | `R r -> (ls, r :: rs)

let rec equal eq xs ys =
  match xs, ys with
  | [], [] ->
      true
  | [], _ :: _
  | _ :: _, [] ->
      false
  | x :: xs, y :: ys ->
      eq x y && equal eq xs ys

let rec compare cmp l1 l2 =
  match l1, l2 with
  | [], [] ->
      0
  | _ :: _, [] ->
      +1
  | [], _ :: _ ->
      -1
  | x1 :: xs1, x2 :: xs2 ->
      let c = cmp x1 x2 in
      if c = 0 then compare cmp xs1 xs2 else c

let hash hash xs =
  Hashtbl.hash (List.map hash xs)
    (* TODO I believe [Hashtbl.hash] inspects the list only down to a certain
       depth, so it is useless to eagerly hash all of the elements. Find some
       other way? Convert to an array, then compute a hash of this array? *)

(* [removeq x xs] returns the list [xs] deprived of the element [x],
   which must physically appear in the list. *)

let rec removeq x xs =
  match xs with
  | [] ->
      assert false
  | x' :: xs ->
      if x == x' then xs else x' :: removeq x xs

let extract (p : 'a -> bool) (xs : 'a list) : 'a option * 'a list =
  match List.find p xs with
  | exception Not_found ->
      None, xs
  | x ->
      let xs = removeq x xs in
      Some x, xs

let rec uniq1 cmp x ys =
  match ys with
  | [] ->
      []
  | y :: ys ->
      if cmp x y = 0 then
        uniq1 cmp x ys
      else
        y :: uniq1 cmp y ys

let uniq cmp xs =
  match xs with
  | [] ->
      []
  | x :: xs ->
      x :: uniq1 cmp x xs

let[@tail_mod_cons] rec merge_uniq cmp l1 l2 =
  match l1, l2 with
  | [], l2 ->
      l2
  | l1, [] ->
      l1
  | h1 :: t1, h2 :: t2 ->
      let c = cmp h1 h2 in
      if c = 0 then
        merge_uniq cmp l1 t2
      else if c < 0 then
        h1 :: merge_uniq cmp t1 l2
      else
        h2 :: merge_uniq cmp l1 t2

(* [reduce_round] divides the length of the list by two. *)

let[@tail_mod_cons] rec reduce_round f xys =
  match xys with
  | x :: y :: xys ->
      f x y :: reduce_round f xys
  | xys ->
      xys

(* [reduce] iterates [reduce_round] until the size of the list
   becomes zero or one. The number of iterations is logarithmic. *)

let rec reduce e f xs =
  match reduce_round f xs with
  | [] ->
      e
  | [x] ->
      x
  | xs ->
      reduce e f xs
