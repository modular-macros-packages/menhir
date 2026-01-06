%token A B
%start<unit> main
%merge { fun _ () () -> () }
%merge { fun _ () () -> () }
%%

main: item {}

item:
  A {}
| B {}
| A {}
%merge { fun () () -> () }
