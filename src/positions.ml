(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Lexing
open Range

type 'a located =
    {
      value    : 'a;
      position : range;
    }

let value { value = v } =
  v

let position { position = p } =
  p

let decompose { value; position } =
  (value, position)

let with_pos p v =
  {
    value     = v;
    position  = p;
  }

let with_loc p v =
  with_pos (Range.make p) v

let map f v =
  {
    value     = f v.value;
    position  = v.position;
  }

let pmap f v =
  {
    value     = f v.position v.value;
    position  = v.position
  }

let unknown_pos v =
  {
    value     = v;
    position  = dummy
  }

let fragment { value; position } =
  let pos1 = startp position
  and pos2 = endp position in
  Fragment.make pos1 pos2 value

(* The functions that print error messages and warnings require a list of
   positions. The following auxiliary functions help build such lists. *)

type positions =
    range list

let one (pos : position) : positions =
  [ make (pos, pos) ]

let print (pos : position) =
  Printf.printf
    "{ pos_fname = \"%s\"; pos_lnum = %d; pos_bol = %d; pos_cnum = %d }\n"
      pos.pos_fname
      pos.pos_lnum
      pos.pos_bol
      pos.pos_cnum
