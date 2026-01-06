%token A B
%start<unit> main
%type<unit> item
%merge { fun _ _ _ -> assert false }
%%

main: item {}

item:
  A {}
| B {}
%merge { fun () () -> () }
