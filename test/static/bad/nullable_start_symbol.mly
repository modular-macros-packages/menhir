%token EOF
%start<unit> main
%%

main: nullable {}

nullable:
|     {}
| EOF {}
