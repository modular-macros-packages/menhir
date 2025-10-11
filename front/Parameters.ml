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
      ParameterApp (p, ps)

let unapp = function
  | ParamVar x ->
      (x, [])
  | ParameterApp (p, ps) ->
      (p, ps)
  | ParameterAnonymous _ ->
      (* Anonymous rules are eliminated early on. *)
      assert false

let unvar = function
  | ParamVar x ->
      x
  | ParameterApp _
  | ParameterAnonymous _ ->
      assert false

let rec map f = function
  | ParamVar x ->
      ParamVar (f x)
  | ParameterApp (p, ps) ->
      ParameterApp (f p, List.map (map f) ps)
  | ParameterAnonymous _ ->
      (* Anonymous rules are eliminated early on. *)
      assert false

let rec fold f init = function
  | ParamVar x ->
      f init x
  | ParameterApp (p, ps) ->
      f (List.fold_left (fold f) init ps) p
  | ParameterAnonymous _ ->
      (* Anonymous rules are eliminated early on. *)
      assert false

let identifiers m p =
  fold (fun accu x -> StringMap.add (value x) (position x) accu) m p

let identifiers ps =
  List.fold_left identifiers StringMap.empty ps

let rec occurs (x : symbol) (p : parameter) =
  match p with
  | ParamVar y ->
      x = value y
  | ParameterApp (y, ps) ->
      x = value y || List.exists (occurs x) ps
  | ParameterAnonymous _ ->
      assert false

let occurs_shallow (x : symbol) (p : parameter) =
  match p with
  | ParamVar y ->
      x = value y
  | ParameterApp (y, _) ->
      assert (x <> value y);
      false
  | ParameterAnonymous _ ->
      assert false

let occurs_deep (x : symbol) (p : parameter) =
  match p with
  | ParamVar _ ->
      false
  | ParameterApp (_, ps) ->
      List.exists (occurs x) ps
  | ParameterAnonymous _ ->
      assert false

type t = parameter

let rec equal x y =
  match x, y with
    | ParamVar x, ParamVar y ->
        value x = value y
    | ParameterApp (p1, p2), ParameterApp (p1', p2') ->
        value p1 = value p1' && List.for_all2 equal p2 p2'
    | _ ->
        (* Anonymous rules are eliminated early on. *)
        false

let hash = function
  | ParamVar x
  | ParameterApp (x, _) ->
      Hashtbl.hash (value x)
  | ParameterAnonymous _ ->
      (* Anonymous rules are eliminated early on. *)
      assert false

let position = function
  | ParamVar x
  | ParameterApp (x, _) ->
      position x
  | ParameterAnonymous bs ->
      position bs

let locate p =
  locate (position p) p

let rec print with_spaces = function
  | ParamVar x
  | ParameterApp (x, []) ->
      value x
  | ParameterApp (x, ps) ->
      let separator = if with_spaces then ", " else "," in
      Printf.sprintf "%s(%s)"
        (value x)
        (MString.separated_list (print with_spaces) separator ps)
  | ParameterAnonymous _ ->
      assert false
