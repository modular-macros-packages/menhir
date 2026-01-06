%token A B
%start<unit> main
%merge { fun _ () () -> ignore $loc }
%%

main: list(item) {}

item:
  A {}
| B {}
| A {}
%merge { fun () () -> ignore $loc }
