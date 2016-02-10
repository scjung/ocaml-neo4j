%{
  open Neo4j_cypher
%}

%start <Neo4j_cypher.t> parse

%%

parse:
| s = statement EOF             { s }
| s = statement SEMICOLON EOF   { s }
;

statement:
| c = command   { Command c }
| q = query     { Query q }
;

query:
| 

(*
  Local variables:
  mode: tuareg
  compile-command: "make -C .."
  End:
 *)
