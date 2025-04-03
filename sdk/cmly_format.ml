(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module defines the data that is stored in .cmly files. *)

(**In short, a .cmly file contains a value of type {!grammar}. *)

(**The type definitions in this module are used in [Cmly_write], which
   writes a .cmly file, and in {!Cmly_read}, which reads a .cmly file.
   They should not be used anywhere else. *)

(**All entities (terminal symbols, nonterminal symbols, and so on) are
   represented as integers. These integers serve as indices into arrays.
   This enables simple and efficient hashing, comparison, indexing, etc. *)

type terminal    = int
type nonterminal = int
type production  = int
type lr0         = int
type lr1         = int

type ocamltype   = string
type ocamlexpr   = string

type range = {
  r_start: Lexing.position;
  r_end: Lexing.position;
}

type attribute = {
  a_label: string;
  a_payload: string;
  a_position: range;
}

type attributes =
  attribute list

type terminal_def = {
  t_name: string;
  t_kind: [`REGULAR | `ERROR | `EOF | `PSEUDO];
  t_type: ocamltype option;
  t_attributes: attributes;
}

type nonterminal_def = {
  n_name: string;
  n_kind: [`REGULAR | `START];
  n_mangled_name: string;
  n_type: ocamltype option;
  n_positions: range list;
  n_nullable: bool;
  n_first: terminal list;
  n_attributes: attributes;
}

type symbol =
  | T of terminal
  | N of nonterminal

type identifier = string

type action = {
  a_expr: ocamlexpr;
  a_keywords: Keyword.keyword list;
}

type producer_def =
  symbol * identifier * attributes

type production_def = {
  p_kind: [`REGULAR | `START];
  p_lhs: nonterminal;
  p_rhs: producer_def array;
  p_positions: range list;
  p_action: action option;
  p_attributes: attributes;
    (* Before 2023/08/02, these were the attributes of the left-hand
       side of the production. Now, these are the attributes of the
       production itself. *)
}

type lr0_state_def = {
  lr0_incoming: symbol option;
  lr0_items: (production * int) list;
}

(**Detailed information on a state of the LR(1) automaton. *)
type lr1_state_def = {

  lr1_lr0: lr0;
  (**The LR(0) state that underlies this state. *)

  lr1_transitions: (symbol * lr1) list;
  (**The outgoing transitions of this state. Each outgoing transition
     isrepresented as a pair of a label (a symbol) and a target state.

     The fields [lr1_transitions] and [lr1_reductions] describe the
     deterministic automaton that is obtained after conflict resolution.
     Therefore, for each terminal symbol [t], there is at most one
     transition or reduction action. *)

  lr1_reductions: (terminal * production) list;
  (**The reduction actions at this state. Each reduction action is
     represented as a pair of a terminal symbol and a production.

     The fields [lr1_transitions] and [lr1_reductions] describe the
     deterministic automaton that is obtained after conflict resolution.
     Therefore, for each terminal symbol [t], there is at most one
     transition or reduction action. *)

  lr1_conflicting_transitions: (terminal * lr1 * bool) list;
  (**The outgoing transitions that have been removed by conflict resolution.
     The Boolean component is true if the conflict was silently resolved by
     obeying the precedence declarations; it is false if the conflict was
     accidental and gave rise to a warning. *)

  lr1_conflicting_reductions: (terminal * production list * bool) list;
  (**The reduction actions that have been removed by conflict resolution.
     The Boolean component is true if the conflict was silently resolved by
     obeying the precedence declarations; it is false if the conflict was
     accidental and gave rise to a warning. *)

  lr1_extra_reductions: (terminal * production) list;
  (**The reduction actions that have been added to obey [%on_error_reduce]
     directives. These reduction actions already appear in [lr1_reductions]. *)

  lr1_default_reduction: production option;
  (**This field is set to [Some prod] if this state reduces production [prod]
     as a default reduction. Regardless of the value of this field, the above
     fields are populated normally. *)

}

type grammar = {
  g_basename     : string;
  g_preludes     : string list;
  g_postludes    : string list;
  g_terminals    : terminal_def    array;
  g_nonterminals : nonterminal_def array;
  g_productions  : production_def  array;
  g_lr0_states   : lr0_state_def   array;
  g_lr1_states   : lr1_state_def   array;
  g_entry_points : (nonterminal * production * lr1) list;
  g_attributes   : attributes;
  g_parameters   : string list;
}
