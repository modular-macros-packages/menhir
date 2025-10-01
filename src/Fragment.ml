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

(**A fragment represents a value together with a range in a source file. *)
type 'a fragment = {

  filename  : string;
  (**The name of the file where this fragment was found. *)

  linenum   : int;
  (**The line number of (the start of) this fragment in the file. *)

  linecount : int;
  (**The number of lines occupied by this fragment in the file. *)

  column    : int;
  (**The column number of (the start of) this fragment in the file. *)

  content   : 'a;
  (**The content of this fragment. *)

}

let make pos1 pos2 content =
  let filename = pos1.pos_fname
  and linenum = pos1.pos_lnum
  and linecount = pos2.pos_lnum - pos1.pos_lnum
  and column = pos1.pos_cnum - pos1.pos_bol in
  { filename; linenum; linecount; column; content }

let parenthesize fragment =
  let { column; content; _ } = fragment in
  let content = "(" ^ content ^ ")"
  and column = max 0 (column - 1) in
  { fragment with column; content }

let content fragment =
  fragment.content

open Positions

let convert v =
  let range = position v in
  make (Range.startp range) (Range.endp range) (value v)
