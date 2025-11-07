(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**This module defines a sum type: either something or a comment (a string).  *)

type comment =
  string

type 'a or_comment =
  | Thing of 'a
  | Comment of comment

type 'a t =
  'a or_comment

(**[iter] enumerates the things (of which there are zero or one). *)
val iter : ('a -> unit) -> 'a t -> unit

(**[fold] enumerates the things (of which there are zero or one). *)
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(**[fold] transforms the things (of which there are zero or one). *)
val map : ('a -> 'b) -> 'a t -> 'b t

(**[to_option] transforms the things (of which there are zero or one)
   into an option. *)
val to_option : 'a t -> 'a option

(**[count] counts the things in a list of things or comments. *)
val count : 'a t list -> int

open Validate

(**[validate] builds a validator. *)
val validate : ('a, 'b) validator -> ('a t, 'b t) validator
