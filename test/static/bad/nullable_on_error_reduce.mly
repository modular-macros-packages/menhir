%token A EOF
%start<unit> main
%on_error_reduce nullable
%%

main: A nullable {}

nullable:
|     {}
| EOF {}
