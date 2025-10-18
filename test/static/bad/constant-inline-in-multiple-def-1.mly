%token A
%start<int> b
%%

b: {}
%public a: A {}
