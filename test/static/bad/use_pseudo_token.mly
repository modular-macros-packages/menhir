%token FOO
%left A

%start<unit> main

%%

main: FOO A+ {}
