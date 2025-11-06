(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module defines a few concrete types for use by [RawSentenceParser]. *)

open Lexing
open Range

type raw_symbol =
  string * position * position

type raw_nonterminal =
  raw_symbol

type raw_terminal =
  raw_symbol

type raw_sentence =
  raw_nonterminal option * raw_terminal list

type located_raw_sentence =
  ranges * raw_sentence

type entry =
  located_raw_sentence OrComment.t list
