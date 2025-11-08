(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open PlainSyntax

module Make (E : sig

  (**[warning ranges format ...] reports a warning. It does not abort. *)
  val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

end) = struct
  open E

  let keep_branch branch : bool =
    let n = List.length branch.producers in
    let warned = ref false in
    let () =
      branch.producers |> List.iteri @@ fun i producer ->
      if i < n - 1
      && producer.prod_symbol = "error"
      && not !warned then
        let () = warned := true in
        warning [branch.branch_position]
          "when --strategy simplified is selected,\n\
           the error token may appear only at the end of a production\n\
           (and the semantic action must abort the parser).\n\
           This production will be ignored."
    in
    not !warned

  let filter_rule rule =
    { rule with branches = List.filter keep_branch rule.branches }

  let filter_grammar grammar =
    { grammar with rules = StringMap.map filter_rule grammar.rules }

end
