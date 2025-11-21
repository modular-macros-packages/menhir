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
open PlainSyntaxAccessors

module Make
(X : sig
  val grammar: grammar
end) ()
= struct
open X

(* -------------------------------------------------------------------------- *)

(* We need to know which symbols generate the empty language. *)

module G = struct
  type nonterminal = string
  type terminal = string
  type symbol = string
  type production = branch
  let classify symbol =
    if is_nonterminal grammar symbol then `N symbol else `T symbol
  let rhs branch =
    List.map (fun producer -> producer.prod_symbol) branch.producers
    |> Array.of_list
  let productions nt =
    let rule = StringMap.find nt grammar.rules in
    rule.branches
  let ignore _prod = false
end

module String = struct
  include String
  let hash = Hashtbl.hash (* [String.hash] appears in OCaml 5.0 *)
end

module NONEMPTY =
  GenericAnalysis.Make
    (Fix.Glue.HashTablesAsImperativeMaps(String))
    (Fix.Prop.Boolean)
    (G)
    (struct
      let shortcut _nt = None
      (* A terminal symbol is nonempty. *)
      let terminal _ = true
      (* An alternative is nonempty if at least one branch is nonempty. *)
      let disjunction p q = p || q()
      (* A sequence is nonempty if both members are nonempty. *)
      let conjunction _ p q = p && q()
      (* The sequence epsilon is nonempty. It generates the singleton
         language {epsilon}. *)
      let epsilon = true
    end)

(* -------------------------------------------------------------------------- *)

(* Transform the grammar. All empty symbols are removed.
   In the remaining definitions, all empty branches are removed. *)

let transform_branches (branches : branches) : branches =
  List.filter NONEMPTY.production branches

let transform_rule (rule : rule) : rule =
  { rule with branches = transform_branches rule.branches }

module M =
  StringMap

let rules =
  M.fold (fun nt rule rules ->
    if NONEMPTY.symbol nt then
      (* If N is nonempty, transform its definition. *)
      rules
      |> M.add nt (transform_rule rule)
    else
      (* If N is empty, throw away its definition. *)
      rules
  ) grammar.rules M.empty

let keep nt _ =
  NONEMPTY.symbol nt

let types =
  M.filter keep grammar.types

let on_error_reduce =
  M.filter keep grammar.on_error_reduce

let grammar =
  { grammar with rules; types; on_error_reduce }

(* If a start symbol generates the empty language, fail. *)

let () =
  grammar.start_symbols |> StringSet.iter @@ fun nt ->
  if not (NONEMPTY.symbol nt) then
    Report.Just.error []
      "The start symbol %s generates the empty language."
      nt

end (* Make *)

(* -------------------------------------------------------------------------- *)

(* Re-package [Make] as a function. *)

let transform grammar =
  let module M = Make(struct
    let grammar = grammar
  end)() in
  M.grammar
