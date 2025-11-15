(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Report

(**[main] is our main channel for reporting errors. *)
val main: channel

(**[logG] is the maximum enabled level of verbosity
   for messages that concern the grammar. *)
val logG : int ref

(**[logA] is the maximum enabled level of verbosity
   for messages that concern the automaton. *)
val logA : int ref

(**[logC] is the maximum enabled level of verbosity
   for messages that concern the generated code. *)
val logC : int ref

(**[getG i] determines which report channel should be used
   for a grammar-related message whose verbosity level is [i]. *)
val getG : int -> channel

(**[getA i] determines which report channel should be used
   for an automaton-related message whose verbosity level is [i]. *)
val getA : int -> channel

(**[getC i] determines which report channel should be used
   for a code-generation-related message whose verbosity level is [i]. *)
val getC : int -> channel
