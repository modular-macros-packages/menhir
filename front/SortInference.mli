(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Syntax
open Sort

(**A sort assignment is a map of (terminal and nonterminal) symbols
   to sorts. *)
type sorts =
  sort StringMap.t

module Make (E : sig

  (**[error ranges format ...] reports an error and aborts. *)
  val error : ranges -> ('a, out_channel, unit, 'b) format4 -> 'a

  (**[log format ...] emits an information message. *)
  val log: ('a, out_channel, unit, unit) format4 -> 'a

end) (V : sig

  (**If [verbose] is [true] then the sort of every symbol is displayed
     through [Error.log]. *)
  val verbose : bool

end) : sig

  (**[infer g] performs sort inference for the grammar [g]. If the grammar is
     well-sorted, it returns a sort assignment. Otherwise, it fails. *)
  val infer: grammar -> sorts

end
