open IL
open ILConstruction

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

let define_and_measure c x e =
  Report.log c
    "The %s table occupies roughly %d bytes."
    x (size e * (Sys.word_size / 8));
  def x e
