(* Reported by Daniel Weil and Radu Grigore. *)
(* Fixed in r154. *)
%token C
%start<unit> c
%%
b(A): A(b) {}
a(B): B(a) {}
c: C a(b) {}
%%
