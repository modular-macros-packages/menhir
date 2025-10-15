(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* [valid_ocaml_identifier] never fails. It returns [true] or [false] to
   indicate whether the input string is or is not a valid OCaml identifier. *)

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)

rule valid_ocaml_identifier = parse
| "_" eof
    { false }
| lowercase identchar* eof
    { true }
| _
| eof
    { false }
