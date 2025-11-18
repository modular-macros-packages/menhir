(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers a generic grammar analysis algorithm, based on a least
   fixed point computation. The grammar is described by four types and four
   functions. The desired analysis is described by just four functions and a
   constant. *)

open Fix

module Make
(M : IMPERATIVE_MAPS)
(P : PROPERTY)
(G : sig

  (* A grammar is described by the following functions. *)

  type nonterminal = M.key
  type terminal
  type symbol
  type production

  (**[classify symbol] determines whether the symbol [symbol] is a
     terminal symbol or a nonterminal symbol. *)
  val classify: symbol -> [`N of nonterminal | `T of terminal]

  (**[rhs prod] is the right-hand side of the production [prod]. *)
  val rhs: production -> symbol array

  (**[productions nt] is the list of the productions of
     the nonterminal symbol [nt]. *)
  val productions: nonterminal -> production list

  (**[ignore prod] indicates whether the production [prod] should be
     ignored during the analysis. *)
  val ignore: production -> bool

end)
(S : sig
  open P
  open G

  (* An analysis is specified by the following functions. *)

  (**If [shortcut nt] is [Some p], then the definition of the nonterminal
     symbol [nt] is ignored by the analysis, and this symbol is mapped by
     the analysis to the property [p]. *)
  val shortcut: nonterminal -> property option

  (**[terminal] maps a terminal symbol to a property. *)
  val terminal: terminal -> property

  (**[disjunction] abstracts a binary alternative. That is, when we analyze
     an alternative between several productions, we compute a property for
     each of them independently, then we combine these properties using
     [disjunction]. *)
  val disjunction: property -> (unit -> property) -> property

  (**[conjunction] abstracts a binary sequence. That is, when we analyze a
     sequence, we compute a property for each member independently, then we
     combine these properties using [conjunction]. In general, conjunction
     needs access to the first member of the sequence (a symbol), not just
     to its analysis (a property).

     The property [P.bottom] must be a neutral element for [disjunction]. We
     use [P.bottom] in the analysis of an alternative with zero branches. *)
  val conjunction: symbol -> property -> (unit -> property) -> property

  (**[epsilon] abstracts the empty sequence. It must be a neutral element
     for [conjunction]. *)
  val epsilon: property

end)
: sig
  open G
  open P

  (* The results of the analysis take the following form. *)

  (* The results are computed on demand, that is, when the functions
     listed below are invoked. *)

  (**To every nonterminal symbol, we associate a property. *)
  val nonterminal: nonterminal -> property

  (**To every symbol, we associate a property. *)
  val symbol: symbol -> property

  (**To every production, we associate a property. *)
  val production: production -> property

  (**To every suffix of every production, we associate a property.
     The offset [i], which determines the beginning of the suffix,
     must be contained between [0] and [n], inclusive, where [n]
     is the length of the production. *)
  val production_suffix: production -> int -> property

end
