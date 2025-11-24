(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module constructs a rich view of the grammar,
   which obeys the signature [GRAMMAR]. *)

open PlainSyntax
open MiddleAPI

(**The functor [Make] transforms a grammar in [PlainSyntax] format
   into a rich internal view. *)
module Make (G : sig
  val grammar: grammar
end)
(X : sig
  val topological_numbering: bool
end)
() : GRAMMAR
