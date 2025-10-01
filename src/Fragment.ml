(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Located

(**A fragment represents a value together with a range in a source file. *)
type 'a fragment = 'a located

let make pos1 pos2 content =
  locate (Range.make (pos1, pos2)) content

let parenthesize fragment =
  let range = position fragment in
  let content = value fragment in
  let content = "(" ^ content ^ ")" in
  locate (Range.decrement range) content

let content =
  value
