%token FOO BAR

%start<unit> foo
%%

a(X):
  x = X { x }

foo:
  a(FOO) a(BAR,BAR) {}
