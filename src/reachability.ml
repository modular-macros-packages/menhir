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

module Run (G : sig val grammar : grammar end) = struct
open G

let rec visit_symbol visited symbol =
  match StringMap.find symbol grammar.rules with
  | exception Not_found ->
      (* This must be a terminal symbol. *)
      visited
  | rule ->
      if StringSet.mem symbol visited then
        visited
      else
        let visited = StringSet.add symbol visited in
        List.fold_left (visit_branch) visited rule.branches

and visit_branch visited { producers = symbols } =
  List.fold_left (visit_producer) visited symbols

and visit_producer visited producer =
  visit_symbol visited producer.prod_symbol

let grammar =
  if StringSet.cardinal grammar.start_symbols = 0 then
    Error.error [] "no start symbol has been declared."
  else
    let reachable =
      StringSet.fold (fun symbol visited ->
        visit_symbol visited symbol
      ) grammar.start_symbols StringSet.empty
    in
    StringMap.iter (fun symbol rule ->
      if not (StringSet.mem symbol reachable) then
        warning rule.positions
          "symbol %s is unreachable from any of the start symbol(s)."
          symbol
    ) grammar.rules;
    { grammar with
      rules = StringMap.restrict reachable grammar.rules;
      types = StringMap.restrict reachable grammar.types;
      on_error_reduce = StringMap.restrict reachable grammar.on_error_reduce;
    }

end (* Run *)

let trim grammar =
  let module R = Run(struct let grammar = grammar end) in
  R.grammar

end (* Make *)
