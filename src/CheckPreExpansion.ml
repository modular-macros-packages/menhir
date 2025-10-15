(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Attribute
open Syntax

module Make (Error : sig

  (**[signal ranges format ...] reports an error, but does not abort.
     Therefore, it is possible to report several errors. *)
  val signal: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

end) = struct

  let valid_ocaml_identifier (x : string) : bool =
    Lexpointfree.valid_ocaml_identifier (Lexing.from_string x)

  let check_attribute attr =
    if attr.key = "name" && not (valid_ocaml_identifier attr.payload) then
      Error.signal [attr.origin]
        "the name of a production must be a valid OCaml identifier"

  let check_branch branch =
    List.iter check_attribute branch.pb_attributes

  let check_rule _nt rule =
    List.iter check_branch rule.pr_branches

  let check grammar =
    StringMap.iter check_rule grammar.p_rules

end
