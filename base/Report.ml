(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

type channel = {
  buffer: Buffer.t;
  mutable count: int;
}

let create () =
  let buffer = Buffer.create 80
  and count = 0 in
  { buffer; count }

let[@inline] count c =
  c.count

let flush f c =
  let b = c.buffer in
  Printf.fprintf f "%s%!" (Buffer.contents b);
  Buffer.clear b

let exit_if c =
  flush stderr c;
  if count c > 0 then exit 1

let monitor (strict : [`Strict | `Lax]) action =
  let c = create() in
  match action c with
  | result ->
      flush stderr c;
      if strict = `Strict && count c > 0 then exit 1;
      result
  | exception e ->
      flush stderr c;
      if strict = `Strict && count c > 0 then exit 1;
      raise e

let display c continuation header positions format =
  c.count <- c.count + 1;
  let b = c.buffer in
  Range.bprint b positions;
  Printf.kbprintf continuation b
    (header ^^ format ^^ "\n%!")

let flush_and_die c _b =
  flush stderr c;
  exit 1

let flush_and_ret c _b =
  flush stderr c

let ret _b =
  ()

let error c positions format =
  display c (flush_and_die c) "Error: " positions format

let signal c positions format =
  display c ret "Error: " positions format

let warning c positions format =
  display c ret "Warning: " positions format

let note c positions format =
  display c ret "Note: " positions format

let log c format =
  let positions = [] in
  display c ret "" positions format

(* One might be tempted to write:

   let error' positions format =
     monitor `Strict @@ fun c ->
     error c positions format

  This code has the desired type, but not the desired semantics:
  the application of this function to the extra arguments that
  follow [format] takes place outside the scope of [monitor], so
  things go wrong. *)

(* Fortunately, there is a simpler definition: *)

let error' positions format =
  let c = create() in
  error c positions format

let warning' positions format =
  let c = create() in
  display c (flush_and_ret c) "Warning: " positions format
