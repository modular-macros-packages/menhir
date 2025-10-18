%token T
%start<unit> b
%%
a(A): A(a) {}
b: T a(a) {}
%%
