(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

let postincrement r =
  let x = !r in
  r := x + 1;
  x

let mkgensym () =
  let r = ref 0 in
  fun () ->
    postincrement r
