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
open Located

(**A fragment represents a value together with a range in a source file. *)
type 'a fragment = 'a located

(**[make pos1 pos2 x] constructs a fragment whose content is the value [v]
   and whose range is given by the positions [pos1] and [pos2]. This call
   is side-effect-free; the file is not accessed. *)
val make : position -> position -> 'a -> 'a fragment

(**[parenthesize f] introduces a pair of parentheses around the (string)
   content of the fragment [f]. The source code range is adjusted by one
   so that the previous content is still printed at the same column. *)
val parenthesize : string fragment -> string fragment

(**[content f] returns the content of the fragment [f]. *)
val content : 'a fragment -> 'a
