%token A B
%start<unit> main
%%

main: list(item) {}
%merge { fun () () -> () }

%inline item:
  A {}
| B {}
%merge { fun () () -> () }
