(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Grammar

(* Concrete syntax trees. *)

(* A concrete syntax tree is one of a leaf -- which corresponds to a
   terminal symbol; a node -- which corresponds to a non-terminal
   symbol, and whose immediate descendants form an expansion of that
   symbol; or an error leaf -- which corresponds to a point where the
   [error] token was shifted. *)

type cst =
  | CstTerminal of Terminal.t
  | CstNonTerminal of Production.t * cst array
  | CstError

      (* A leaf is denoted by a terminal symbol. *)

      (* A node is denoted by a bracketed, whitespace-separated list,
         whose head is a non-terminal symbol (followed with a colon)
         and whose tail consists of the node's descendants. *)

      (* There is in fact some ambiguity in this notation, since we
         only print the non-terminal symbol that forms the left-hand
         side of production [prod], instead of the production itself.

         This abuse makes things much more readable, and should be
         acceptable for the moment. The cases where ambiguity actually
         arises should be rare. *)

      (* An error leaf is denoted by [error]. *)

(* This is a pretty-printer for concrete syntax trees. The notation is
   the same as that used by the above printer; the only difference is
   that the [PPrint] library is used to manage indentation. *)

open PPrint

let rec build : cst -> document = function
  | CstTerminal tok ->
      string (Terminal.print tok)
  | CstNonTerminal (prod, csts) ->
      brackets (
        group (
          string (Nonterminal.print false (Production.nt prod)) ^^
          colon ^^
          group (
            nest 2 (
              Array.fold_left (fun doc cst ->
                doc ^/^ build cst
              ) empty csts
            )
          ) ^^
          break 0
        )
      )
  | CstError ->
      string "error"

let show f cst =
  ToChannel.pretty 0.8 80 f (build cst)
