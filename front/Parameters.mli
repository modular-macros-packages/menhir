(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module offers helper functions to deal with the type [parameter].

   Because the constructor [ParamAnonynous] is eliminated very early on, most
   of these functions cannot deal with this constructor. The only functions
   that can deal with it are {!apply}, {!locate}, and {!position}. *)

open Syntax

(**[apply s ps] constructs an application of the symbol [s] to the
   parameters [ps]. The list [ps] can be empty. This smart constructor
   should be preferred to a direct use of the constructor [ParamApp],
   which does not accept an empty list. *)
val apply : symbol located -> parameters -> parameter

(**[destruct p] destructs the parameter [p] into an application of a symbol
   [s] to a list of parameters [ps]. *)
val destruct : parameter -> symbol located * parameters

(**[iter f p] applies the action [f] to every symbol that appears in the
   parameter [p]. *)
val iter : (symbol located -> unit) -> parameter -> unit

(**[map f p] applies the transformation [f] to every symbol that appears in
   the parameter [p]. *)
val map : (symbol -> symbol) -> parameter -> parameter

val occurs : symbol -> parameter -> bool
val occurs_shallow : symbol -> parameter -> bool
val occurs_deep : symbol -> parameter -> bool

type t = parameter
val equal : t -> t -> bool
val hash : t -> int
val position : parameter -> range
val locate : parameter -> parameter located
val print : bool -> parameter -> string
