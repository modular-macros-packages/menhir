(* An OCaml type is either a thing (if it was found in some
   source file) or a string (if it was inferred via [Infer]). *)

open Fragment

type ocamltype =
  | Declared of string fragment
  | Inferred of string
