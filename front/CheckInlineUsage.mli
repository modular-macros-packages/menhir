(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module checks that the grammar does not use %inline rules. This check
   is performed only when the Coq back-end has been selected by the user. *)

open PlainSyntax

module Make (E : sig

  (**[error ranges format ...] reports an error and aborts. *)
  val error : ranges -> ('a, out_channel, unit, 'b) format4 -> 'a

end) : sig

  (**[check] checks that the grammar does not use %inline rules. *)
  val check: grammar -> unit

end
