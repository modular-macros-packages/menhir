(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open MiddleAPI

(* This module first constructs an LR(1) automaton by using an appropriate
   construction method (LALR, Pager, canonical).
   Then, this automaton is further transformed (in place), in three steps:

   1. Silent conflict resolution (without warnings),
      following the user's precedence declarations.
      This is done immediately.
      This can remove transitions and reductions.

   2. Default conflict resolution (with warnings),
      following a fixed default policy.
      This is done via an explicit call to [default_conflict_resolution()].
      This can remove reductions.

   3. Addition of extra reductions,
      following the user's [%on_error_reduce] declarations.
      This is done via an explicit call to [extra_reductions()].

   Conflicts are explained after step 1, and before steps 2 and 3.
   This is the main reason why these steps are separate. *)

(* During step 1, shift/reduce conflicts are silently resolved if (and only
   if) that is allowed in a clean way by user-specified priorities. This
   includes multi-way shift/reduce/reduce conflicts if (and only if) there is
   agreement that the shift action should be preferred. Conflicts that cannot
   be silently resolved in this phase are reported, explained, then
   arbitrarily resolved in step 2. *)

include LR1 with module Lr0 = Lr0
