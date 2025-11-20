%token A B
%start<unit> main
%nonassoc bar
%%

main:
  A optional_B {}

optional_B:
| %prec bar {}
| B         {}
