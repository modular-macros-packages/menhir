%token A
%start<int> a
%%

a: A b {}
%inline b: c {}
%inline c: b {}
