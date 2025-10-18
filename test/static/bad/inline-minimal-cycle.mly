%token A
%start<int> a
%%

a: A b {}
%inline b: b {}
