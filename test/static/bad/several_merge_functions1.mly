%token EOF
%start<unit> main
%merge { fun _nt _v1 _v2 -> failwith "merge" }
%%
main: foo EOF {}
