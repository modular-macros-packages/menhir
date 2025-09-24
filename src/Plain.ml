(******************************************************************************)
(*                                                                            *)
(*                                  IntPQueue                                 *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

let fail format =
  Printf.ksprintf invalid_arg format

type priority =
  int

module MyArray = Vector
module MyStack = Vector

(* We use vectors to represent both the main array and the secondary
   stacks. These two uses are in principle independent of one another. *)

(* Hector's vectors have a memory leak: after an element has been extracted by
   [pop], the vector can still contain a pointer to this element. Thus, our
   priority queue, too, has a memory leak. In practice, we do not expect this
   to create a problem. The use of [MyStack.reset] in [extract] partially
   mitigates this problem. *)

(* -------------------------------------------------------------------------- *)

(**A priority queue. *)
type 'a t = {

  (* A priority queue is represented as a vector, indexed by priorities, of
     stacks. There is no bound on the size of the main vector -- its size is
     increased if needed. It is up to the user to use priorities of reasonable
     magnitude. *)
  mutable a: 'a MyStack.t MyArray.t;

  (* The index [best] is comprised between 0 (included) and the length of the
     array [a] (excluded). It can be the index of the lowest nonempty stack,
     if there is one; or it can be lower. In other words, from the index 0
     to the index [best] (excluded), every stack is empty. *)
  mutable best: int;

  (* Current number of elements in the queue. Used in [extract] to stop the
     search for a nonempty bucket. *)
  mutable cardinal: int;

}

(* -------------------------------------------------------------------------- *)

(* Checking well-formedness. (Debugging only.) *)

let check q =
  assert (0 <= q.best && q.best <= MyArray.length q.a);
  for i = 0 to q.best - 1 do
    let xs = MyArray.get q.a i in
    assert (MyStack.length xs = 0);
  done;
  let c = ref 0 in
  for i = q.best to MyArray.length q.a - 1 do
    let xs = MyArray.get q.a i in
    c := !c + MyStack.length xs
  done;
  assert (q.cardinal = !c)

(* -------------------------------------------------------------------------- *)

(* Operations on queues. *)

(* When the main array is created or extended, each level must be initialized
   with a fresh empty stack. [fresh_segment] creates an array of [n] fresh
   empty stacks. *)

let fresh_stack (_j : int) =
  MyStack.create()

let create () =
  let a = MyArray.init 16 fresh_stack in
  { a; best = 0; cardinal = 0 }

let reset q =
  q.a <- MyArray.init 16 fresh_stack;
  q.best <- 0;
  q.cardinal <- 0

let[@inline] grow q i =
  assert (0 <= i);
  let desired = i + 1 in
  let current = MyArray.length q.a in
  if current < desired then begin
    MyArray.ensure_capacity q.a desired;
    MyArray.push_array q.a (Array.init (desired - current) fresh_stack);
  end

let add q x i =
  if i < 0 then fail "add: negative priority (%d)" i;
  q.cardinal <- q.cardinal + 1;
  (* Grow the main array if necessary. *)
  grow q i;
  assert (i < MyArray.length q.a);
  (* Find out which stack we should push into. *)
  let xs = MyArray.unsafe_get q.a i in
  (* Push. *)
  MyStack.push xs x;
  (* Decrease [q.best], if necessary, so as not to miss the new element. In
     the special case of Dijkstra's algorithm or A*, this never happens. *)
  if i < q.best then
    q.best <- i

let[@inline] is_empty q =
  q.cardinal = 0

let[@inline] cardinal q =
  q.cardinal

let rec extract_nonempty q =
  assert (0 < q.cardinal);
  let i = q.best in
  assert (0 <= i && i < MyArray.length q.a);
  (* Look for the next nonempty bucket. We know there is one. This may seem
     inefficient, because it is a linear search. However, in applications
     where [q.best] never decreases, the cumulated cost of this loop is the
     maximum priority ever used, which is good. *)
  let xs = MyArray.unsafe_get q.a i in
  if MyStack.length xs = 0 then begin
    (* As noted below, [MyStack.pop] does not physically shrink the stack.
       When we find that a priority level has become empty, we physically
       empty it, so as to free the (possibly large) space that it takes up.
       This strategy is good when the client is Dijkstra's algorithm or A*. *)
    MyStack.reset xs;
    q.best <- i + 1;
    extract_nonempty q
  end
  else begin
    q.cardinal <- q.cardinal - 1;
    MyStack.pop xs
    (* Note: [MyStack.pop] does not shrink the physical array underlying the
       stack. This is good, because we are likely to push new elements into
       this stack. *)
  end

let[@inline] extract q =
  if q.cardinal = 0 then
    None
  else
    Some (extract_nonempty q)

(* [extract'] is a copy of [extract] where we return a pair [x, i].
   One could define [extract] in terms of [extract'], removing this
   duplication, but this would cause an extra pair allocation. *)

let rec extract'_nonempty q =
  assert (0 < q.cardinal);
  let i = q.best in
  assert (0 <= i && i < MyArray.length q.a);
  let xs = MyArray.unsafe_get q.a i in
  if MyStack.length xs = 0 then begin
    MyStack.reset xs;
    q.best <- i + 1;
    extract'_nonempty q
  end
  else begin
    q.cardinal <- q.cardinal - 1;
    let x = MyStack.pop xs in
    x, i
  end

let[@inline] extract' q =
  if q.cardinal = 0 then
    None
  else
    Some (extract'_nonempty q)

let repeat q f =
  while q.cardinal > 0 do
    let x = extract_nonempty q in
    f x
  done
