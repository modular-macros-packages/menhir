(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Range

type 'a located =
  { p : range; v : 'a }

let[@inline] position { p } = p
let[@inline] value    { v } = v

let locate p v = { p; v}

let with_loc p v =
  locate (Range.make p) v

let map f { p; v } =
  let v = f v in { p; v }

let pmap f { p; v } =
  let v = f p v in { p; v }

let xmap f { p; v } =
  let v = f { p; v } in { p; v }

let unknown_pos v =
  locate dummy v

let fragment { v; p } =
  let pos1 = startp p
  and pos2 = endp p in
  Fragment.make pos1 pos2 v
