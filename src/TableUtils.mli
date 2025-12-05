(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open IL
open Report

(**This module offers support for marshaling data (typically, tables of
   various kinds) as IL code. *)

(**[define_and_measure c x e] prints a measure of the table encoded by the
   expression [e] on the report channel [c], then returns [def x e]. *)
val define_and_measure: channel -> var -> expr -> valdef

(**[marshal1 c name x a] marshals the one-dimensional array [a]. The result is
   a list of value definitions, culminating in an accessor function [x : int
   -> int]. A measure of the table, with the name [name], is printed on the
   report channel [c]. *)
val marshal1:
  channel -> string ->
  var -> int array ->
  valdef list

(**[marshal11] marshals a one-dimensional bitmap. By bitmap, we mean an array
   such that every value stored in the array is 0 or 1. The data is encoded as
   just a string. It can be accessed using [PackedIntArray.get1]. *)
val marshal11: int array -> expr

(**[linearize_and_marshal1] marshals an irregular array of arrays, that is, an
   array of arrays of possibly different lengths. The result is a list of
   value definitions, culminating in an accessor function [x : int -> int ->
   int]. A measure of the table, with the name [name], is printed on the
   report channel [c]. *)
val linearize_and_marshal1:
  channel -> string ->
  var -> int array array ->
  valdef list

(**[flatten_and_marshal11 table] marshals the two-dimensional bitmap [table].
   The result is a list of value definitions, culminating in an accessor
   function [x : int -> int -> int]. A measure of the table, with the name
   [name], is printed on the report channel [c]. *)
val flatten_and_marshal11:
  channel -> string ->
  var -> int array list ->
  valdef list

(**[marshal2 c name x insignificant] marshals a two-dimensional table. The
   parameter [insignificant] determines which table entries are insignificant.
   The result is a list of value definitions, culminating in an accessor
   function [x : int -> int -> int]. Statistic about the table, with the name
   [name], are printed on the report channel [c]. *)
val marshal2:
  channel -> string ->
  var -> (int -> bool) ->
  int array array ->
  valdef list
