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

(**[marshal1] marshals a one-dimensional array. The data is encoded as a pair
   of an integer bit width and a string. *)
val marshal1: int array -> expr

(**[marshal1_specialized c name x a] marshals the one-dimensional array [a].
   The result is a list of value definitions, culminating in an accessor
   function [x_get : int -> int]. A measure of the table, with the name
   [name], is printed on the report channel [c]. *)
val marshal1_specialized:
  channel -> string ->
  var -> int array ->
  valdef list

(**[marshal11] marshals a one-dimensional bitmap. By bitmap, we mean an array
   such that every value stored in the array is 0 or 1. The data is encoded as
   just a string. *)
val marshal11: int array -> expr

(**[linearize_and_marshal1] marshals an array of integer arrays (of possibly
   different lengths). The data is first flattened via [LinearizedArray],
   yielding a pair of one-dimensional arrays. Then each of these arrays is
   marshaled via [marshal1]. *)
val linearize_and_marshal1: int array array -> expr

(**[flatten_and_marshal11 table] marshals the two-dimensional bitmap [table]. *)
val flatten_and_marshal11: int array list -> expr

(**[flatten_and_marshal11_specialized table] marshals the two-dimensional
   bitmap [table]. The result is a list of value definitions, culminating in
   an accessor function [x_get : int -> int -> int]. A measure of the table,
   with the name [name], is printed on the report channel [c]. *)
val flatten_and_marshal11_specialized:
  channel -> string ->
  var -> int array list ->
  valdef list

(**[marshal2 c name insignificant] marshals a two-dimensional table. One line
   of statistics is logged on the report channel [c]; the name [name] is used
   as part of this report. The parameter [insignificant] determines which
   table entries are insignificant. Row displacement is allowed to overlap one
   or more insignificant entries with a significant entry. The data is encoded
   as a pair of arrays, each of which is marshaled via [marshal1]. *)
val marshal2: channel -> string -> (int -> bool) -> int array array -> expr

(**[marshal2_specialized c name x insignificant] marshals a two-dimensional
   table. The parameter [insignificant] determines which table entries are
   insignificant. The result is a list of value definitions, culminating in an
   accessor function [x_get : int -> int -> int]. Statistic about the table,
   with the name [name], are printed on the report channel [c]. *)
val marshal2_specialized:
  channel -> string ->
  var -> (int -> bool) ->
  int array array ->
  valdef list
