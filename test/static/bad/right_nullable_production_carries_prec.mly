%token A B
%start<unit> main
%nonassoc bar
%%

main:
  A B? %prec bar {}
