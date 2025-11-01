(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* This module performs a single call to [StackSymbols.Short()]. *)

open MiddleAPI

include STACK_SYMBOLS
with module G = Grammar
 and type node = Lr1.node
