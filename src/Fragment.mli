
(**A fragment represents a value together with a range in a source file. *)
type 'a fragment = {

  filename  : string;
  (**The name of the file where this fragment was found. *)

  linenum   : int;
  (**The line number of (the start of) this fragment in the file. *)

  linecount : int;
  (**The number of lines occupied by this fragment in the file. *)

  column    : int;
  (**The column number of (the start of) this fragment in the file. *)

  content   : 'a;
  (**The content of this fragment. *)

}

(**[make pos1 pos2 x] constructs a fragment whose content is the value [v]
   and whose range is given by the positions [pos1] and [pos2]. This call
   is side-effect-free; the file is not accessed. *)
val make : Lexing.position -> Lexing.position -> 'a -> 'a fragment

(**[parenthesize f] introduces a pair of parentheses around the (string)
   content of the fragment [f]. The source code range is adjusted by one
   so that the previous content is still printed at the same column. *)
val parenthesize : string fragment -> string fragment

(**[content f] returns the content of the fragment [f]. *)
val content : 'a fragment -> 'a
