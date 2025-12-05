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

(**[marshal_1D_array c name x a] marshals the one-dimensional array [a]. The
   result is a list of value definitions, culminating in an accessor function
   [x : int -> int]. A measure of the table, with the name [name], is printed
   on the report channel [c]. *)
val marshal_1D_array:
  channel -> string ->
  var -> int array ->
  valdef list

(* unused:
(**[marshal_1D_bitmap] marshals a one-dimensional bitmap. By bitmap, we mean
   an array such that every value stored in the array is 0 or 1. The data is
   encoded as just a string. It can be accessed using
   [PackedIntArray.get1]. *)
val marshal_1D_bitmap: int array -> expr
 *)

(**[marshal_irregular_2D_array] marshals an irregular array of arrays, i.e.,
   an array of arrays of possibly different lengths. It returns a list of
   definitions, culminating in an accessor function [x : int -> int -> int].
   A measure of the table, with the name [name], is printed on the report
   channel [c]. *)
val marshal_irregular_2D_array:
  channel -> string ->
  var -> int array array ->
  valdef list

(**[marshal_2D_bitmap] marshals a two-dimensional bitmap, that is, a 2D
   matrix whose elements are 0 or 1. The result is a list of definitions,
   culminating in an accessor function [x : int -> int -> int]. A measure of
   the table, with the name [name], is printed on the report channel [c]. *)
val marshal_2D_bitmap:
  channel -> string ->
  var -> int array list ->
  valdef list

(**[marshal_2D_matrix c name x insignificant] marshals a sparse two-dimensional
   matrix. The parameter [insignificant] determines which table entries are
   insignificant. The result is a list of value definitions, culminating in an
   accessor function [x : int -> int -> int]. Statistics about the table, with
   the name [name], are printed on the report channel [c]. *)
val marshal_2D_matrix:
  channel -> string ->
  var -> (int -> bool) ->
  int array array ->
  valdef list
