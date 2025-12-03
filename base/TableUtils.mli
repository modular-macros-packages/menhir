open IL
open Report

(**[define_and_measure c x e] prints a measure of the table encoded by the
   expression [e] on the report channel [c], then returns [def x e]. *)
val define_and_measure: channel -> var -> expr -> valdef
