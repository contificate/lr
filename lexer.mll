{
  open Parser
}

rule tokenise = parse
| [' ' '\t' '\n']+
    { tokenise lexbuf }
| "->"
    { ARROW }
| '.'
    { DOT }
| ['A'-'Z']+ "'"* as i
    { NONTERMINAL i }
| ['a'-'z' '(' ')' '{' '}' ',' '+' '*' '-']+ as i
    { TERMINAL i }
| eof
    { EOF }