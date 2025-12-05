(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open IL
open ILConstruction
module LinearizedArray = MenhirLib.LinearizedArray
module PackedIntArray = MenhirLib.PackedIntArray

(* -------------------------------------------------------------------------- *)

(* Statistics. *)

(* Integer division, rounded up. *)

let div a b =
  if a mod b = 0 then a / b else a / b + 1

(* [size unboxed e] provides a rough measure of the size of the data encoded
   by the IL expression [e], in words. The parameter [unboxed] is true if we
   have already counted 1 for the pointer to the object. *)

let rec size unboxed (e : expr) =
  match e with
  | EIntConst _
  | ETuple []
  | EData (_, []) ->
      if unboxed then 0 else 1
  | EStringConst s ->
      1 + div (String.length s * 8) Sys.word_size
  | ETuple es
  | EData (_, es)
  | EArray es ->
      1 + List.length es + List.fold_left (fun s e -> s + size true e) 0 es
  | _ ->
      assert false (* not implemented *)

let size e =
  size false e

let measure c name def =
  Report.log c
    "The %s occupies roughly %d bytes."
    name (size def.valval * (Sys.word_size / 8));
  def

let define_and_measure c x e =
  measure c (x ^ " table") (def x e)

(* -------------------------------------------------------------------------- *)

(* Operations on 2D matrices. *)

let matrix_fold_left f accu matrix =
  Array.fold_left (fun accu row ->
    Array.fold_left f accu row
  ) accu matrix

let matrix_height matrix =
  Array.length matrix

let matrix_width matrix =
  assert (matrix_height matrix > 0);
  Array.length matrix.(0)

let matrix_size matrix =
  let m = matrix_height matrix in
  if m = 0 then
    0
  else
    let n = matrix_width matrix in
    m * n

(* [population] counts the number of significant entries
   in a two-dimensional matrix. *)

let population insignificant (matrix : int array array) =
  matrix_fold_left (fun population entry ->
    if insignificant entry then population else population + 1
  ) 0 matrix

(* -------------------------------------------------------------------------- *)

(* Code generation for table accessors. *)

let binop op e1 e2 =
  EApp (EVar op, [ e1; e2])

let add =
  binop "(+)"

let mul =
  binop "(*)"

(* [unflatten n get data] generates code for the expression
   [fun i j -> get data (n * i + j)].
   This transforms an access into a 2D matrix into an access into a 1D array. *)

let unflatten (n : int) (get : var) (data : var) : expr =
  let i = "i" and j = "j" in
  efun [ PVar i; PVar j] @@
  EApp (EVar get, [
    EVar data;
    add (mul (EIntConst n) (EVar i)) (EVar j)
  ])

(* -------------------------------------------------------------------------- *)

(* Table compression. *)

(* We say that a matrix is sparse when some of its entries are insignificant.
   An entry is insignificant if it is never accessed. *)

(* A sparse two-dimensional table is turned into a one-dimensional table
   via [compress]. *)

(* [compress] is our preferred table compression algorithm. It uses either
   [RowDisplacementEncodeClassic] or [RowDisplacementEncode]. These two
   algorithms produce compressed tables in a common format, so they can be
   used interchangeably. *)

(* A non-sparse two-dimensional table can be made one-dimensional by simple
   flattening; see [LinearizedArray]. *)

(* Every one-dimensional table is then packed via [PackedIntArray]. *)

let compress =
  if Settings.pack_classic then
    RowDisplacementEncodeClassic.compress
  else
    RowDisplacementEncode.compress

let marshal1 (table : int array) : expr =
  let (bits : int), (text : string) = PackedIntArray.pack table in
  ETuple [ EIntConst bits; EStringConst text ]

let marshal1_specialized c name x table : valdef list =
  let (bits : int), (text : string) = PackedIntArray.pack table in
  let x_data = sprintf "%s_data" x in
  [
    (* Define the table [x_data]. *)
    measure c name @@
    def x_data @@
    (EStringConst text);
    (* Define the accessor function [x_get = fun i -> get x_data i], where
       [get] is a variant of [PackedIntArray.get] with a suitable bit width. *)
    def_inline (sprintf "%s_get" x) @@
    let i = "i" in
    efun [PVar i] @@
    EApp (
      EVar (sprintf "MenhirLib.PackedIntArray.get%d" bits),
      [ EVar x_data; EVar i ]
    )
  ]

let marshal11 (table : int array) =
  let (bits : int), (text : string) = PackedIntArray.pack table in
  assert (bits = 1);
  EStringConst text

let linearize_and_marshal1 (table : int array array) =
  let data, entry = LinearizedArray.make table in
  ETuple [ marshal1 data; marshal1 entry ]

let flatten_and_marshal11 (table : int array list) =
  assert (List.length table > 0);
  let n = Array.length (List.hd table) in
  assert (List.for_all (fun row -> Array.length row = n) table);
  ETuple [
    (* Store the table width. *)
    EIntConst n;
    (* View the table as a one-dimensional array, and marshal it. *)
    marshal11 (Array.concat table)
  ]

let flatten_and_marshal11_specialized c name x table : valdef list =
  assert (List.length table > 0);
  let n = Array.length (List.hd table) in
  assert (List.for_all (fun row -> Array.length row = n) table);
  let data = sprintf "%s_data" x in
  [
    (* View the table as a one-dimensional array, and marshal it. *)
    measure c name @@ def
      data
      (marshal11 (Array.concat table));
    (* Use the table width [n] in the definition of a specialized accessor
       for the [data] array. The bit width is 1. *)
    def
      (sprintf "%s_get" x)
      (unflatten n "MenhirLib.PackedIntArray.get1" data)
  ]

let marshal2 c name insignificant (matrix : int array array) : expr =
  let dummy = 0 in
  let displacement, data = compress insignificant dummy matrix in
  Report.log c
    "The %s table is %d entries; %d non-zero; %d compressed."
    name (matrix_size matrix) (population insignificant matrix)
    (Array.length displacement + Array.length data);
  ETuple [
    marshal1 displacement;
    marshal1 data;
  ]

(* [row_displacement_decode get_displacement get_data] generates
   the IL expression:
     fun i j ->
       let k = RowDisplacementDecode.decode (get_displacement i) in
       get_data (k + j)
   This is a copy of [RowDisplacementDecode.get],
   specialized with [get_displacement] and [get_data]. *)

let row_displacement_decode get_displacement get_data : expr =
  let decode = "MenhirLib.RowDisplacementDecode.decode" in
  let i = "i" and j = "j" and k = "k" in
  efun [ PVar i; PVar j] @@
  blet (
    [ PVar k, EApp (EVar decode, [ EApp (EVar get_displacement, [ EVar i ])]) ],
    EApp (EVar get_data, [ add (EVar k) (EVar j) ])
  )

let marshal2_specialized c name x insignificant matrix : valdef list =
  let dummy = 0 in
  let displacement, data = compress insignificant dummy matrix in
  Report.log c
    "The %s table is %d entries; %d non-zero; %d compressed."
    name (matrix_size matrix) (population insignificant matrix)
    (Array.length displacement + Array.length data)
  ;
  (* Define an accessor function for the displacement table. *)
  marshal1_specialized c (name ^ " displacement") (x ^ "_displacement") displacement @
  (* Define an accessor function for the data table. *)
  marshal1_specialized c (name ^ " data") (x ^ "_data") data @
  (* Combine the two to obtain an accessor function [x] for the matrix. *)
  [
    def x @@
    row_displacement_decode
      (name ^ "_displacement_get")
      (name ^ "_data_get")
  ]
