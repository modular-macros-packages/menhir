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

end (* Make *)
