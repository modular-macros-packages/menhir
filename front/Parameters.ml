(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Located
open Syntax

let apply p ps =
  match ps with
  | [] ->
      ParamVar p
  | _ ->
      ParamApp (p, ps)

let destruct p =
  match p with
  | ParamVar x ->
      x, []
  | ParamApp (x, ps) ->
      x, ps
  | ParamAnonymous _ ->
      assert false

let head p =
  let x, ps = destruct p in
  assert (ps = []);
  x

let rec iter f p =
  match p with
  | ParamVar x ->
      f x
  | ParamApp (x, ps) ->
      f x;
      List.iter (iter f) ps
  | ParamAnonymous _ ->
      assert false

let rec map f p =
  match p with
  | ParamVar x ->
      ParamVar (Located.map f x)
  | ParamApp (x, ps) ->
      ParamApp (Located.map f x, List.map (map f) ps)
  | ParamAnonymous _ ->
      assert false

let rec occurs x p =
  match p with
  | ParamVar y ->
      x = value y
  | ParamApp (y, ps) ->
      x = value y || List.exists (occurs x) ps
  | ParamAnonymous _ ->
      assert false

let rec equal x y =
  match x, y with
    | ParamVar x, ParamVar y ->
        value x = value y
    | ParamApp (p1, p2), ParamApp (p1', p2') ->
        value p1 = value p1' && List.for_all2 equal p2 p2'
    | _ ->
        false

let hash p =
  match p with
  | ParamVar x
  | ParamApp (x, _) ->
      Hashtbl.hash (value x)
  | ParamAnonymous _ ->
      assert false

(* [position] is not exposed to the user, but is used by [locate]. It must
   accept an arbitrary parameter, which can be [ParamAnonymous _]. *)

let position p =
  match p with
  | ParamVar x
  | ParamApp (x, _) ->
      position x
  | ParamAnonymous bs ->
      position bs

let locate p =
  locate (position p) p

let rec print separator p =
  match p with
  | ParamVar x
  | ParamApp (x, []) ->
      value x
  | ParamApp (x, ps) ->
      Printf.sprintf "%s(%s)"
        (value x)
        (MString.separated_list (print separator) separator ps)
  | ParamAnonymous _ ->
      assert false
