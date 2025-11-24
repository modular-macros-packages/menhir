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

module Make (X : sig val grammar: grammar end) = struct
  open X

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

  include
    GenericAnalysis.Make
      (Fix.Glue.HashTablesAsImperativeMaps(String))
      (Fix.Prop.Boolean)
      (G)
      (struct
        let shortcut _nt = None
        (* A terminal symbol generates a non-epsilon word. *)
        let terminal _ = true
        (* An alternative generates a non-epsilon word if one of the
           branches generates a non-epsilon word. *)
        let disjunction p q = p || q()
        (* A sequence generates a non-epsilon word if one the components
           generates a non-epsilon word. (We assume that both components
           are non-void. Otherwise we obtain an over-approximation.) *)
        let conjunction _ p q = p || q()
        (* The empty sequence does not generate a non-epsilon word. *)
        let epsilon = false
      end)

end (* Make *)
