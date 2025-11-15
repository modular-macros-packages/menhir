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

let keep_branch c branch : bool =
  let n = List.length branch.producers in
  let warned = ref false in
  let () =
    branch.producers |> List.iteri @@ fun i producer ->
    if i < n - 1
    && producer.prod_symbol = "error"
    && not !warned then
      let () = warned := true in
      Report.warning c [branch.branch_position]
        "when --strategy simplified is selected,\n\
         the error token may appear only at the end of a production\n\
         (and the semantic action must abort the parser).\n\
         This production will be ignored."
  in
  not !warned

let filter_rule c rule =
  { rule with branches = List.filter (keep_branch c) rule.branches }

let filter_grammar c grammar =
  { grammar with rules = StringMap.map (filter_rule c) grammar.rules }
