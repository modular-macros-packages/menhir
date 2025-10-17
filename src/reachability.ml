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

  (**[error ranges format ...] reports an error and aborts. *)
  val error : ranges -> ('a, out_channel, unit, 'b) format4 -> 'a

  (**[warning ranges format ...] reports a warning. It does not abort. *)
  val warning: ranges -> ('a, out_channel, unit, unit) format4 -> 'a

end) = struct
open E

module Run (G : sig val grammar : grammar end) = struct
open G

(* Check that at least one start symbol has been declared: otherwise, the
   resulting grammar would be empty. This check cannot be performed earlier
   (e.g., as part of CheckGrammar) because, when [--only-tokens] is used,
   we must tolerate the absence of a start symbol. *)

let () =
  if StringSet.is_empty grammar.start_symbols then
    error [] "no start symbol has been declared."

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

let reachable =
  StringSet.fold (fun symbol visited ->
    visit_symbol visited symbol
  ) grammar.start_symbols StringSet.empty

let roots =
  if StringSet.cardinal grammar.start_symbols = 1 then
    "the start symbol"
  else
    "any of the start symbols"

let () =
  grammar.rules |> StringMap.iter @@ fun symbol rule ->
  if not (StringSet.mem symbol reachable) then
    warning rule.positions
      "symbol %s is unreachable from %s."
      symbol roots

let grammar =
  { grammar with
    rules = StringMap.restrict reachable grammar.rules;
    types = StringMap.restrict reachable grammar.types;
    on_error_reduce = StringMap.restrict reachable grammar.on_error_reduce }

end (* Run *)

let trim grammar =
  let module R = Run(struct let grammar = grammar end) in
  R.grammar

end (* Make *)
