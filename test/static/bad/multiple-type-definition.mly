%token A
%type<int> a
%type<bool> a
%start<int> b
%%

a: A {}
b: {}
