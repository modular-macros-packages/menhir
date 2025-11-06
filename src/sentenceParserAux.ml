(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

type raw_symbol =
  string * Lexing.position * Lexing.position

type raw_nonterminal =
  raw_symbol

type raw_terminal =
  raw_symbol

type raw_sentence =
  raw_nonterminal option * raw_terminal list

type located_raw_sentence =
  Range.ranges * raw_sentence
