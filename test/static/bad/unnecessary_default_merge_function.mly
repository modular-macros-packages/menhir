%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%start<unit> main
%merge { fun _ _ _ -> () }
%%

main:
| expr EOL
    {}
%merge { fun _ _ -> () }

expr:
| INT
| LPAREN expr RPAREN
| expr binop expr
| unop expr
    {}
%merge { fun _ _ -> () }

%inline unop:
  MINUS { }

%inline binop:
  PLUS  { }
| MINUS { }
| TIMES { }
| DIV   { }
