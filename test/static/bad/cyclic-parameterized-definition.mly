(* Reported by Daniel Weil and Radu Grigore. *)
(* Fixed in r154. *)
%start<unit> c
%%
b(A): A(b) {}
a(B): B(a) {}
c: a(b) {}
%%
