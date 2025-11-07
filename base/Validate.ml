(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Report

exception Invalid

type ('a, 'b) validator =
  channel -> 'a -> 'b (* may raise [Invalid] *)

let robustly (validate : ('a, 'b) validator) : ('a, 'b option) validator =
fun c x ->
  try Some (validate c x) with Invalid -> None

let list (validate : ('a, 'b) validator)
: ('a list, 'b list) validator =
fun c xs ->
  xs |> MList.filter_map @@ fun x ->
  robustly validate c x

let apply strict (validate : ('a, 'b) validator) : 'a -> 'b =
fun x ->
  monitor strict @@ fun c ->
  validate c x
