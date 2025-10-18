%token A
%start<int> a
%%

a: A %prec F {}
