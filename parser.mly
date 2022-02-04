
%token<string> NONTERMINAL
%token<string> TERMINAL
%token ARROW DOT EOF

%type<Grammar.symbol> symbol
%type<string * Grammar.symbol list> prod
%start<(string * Grammar.symbol list) list> rules

%%

rules:
  | p = list(prod) EOF
    { p }

prod:
  | x = NONTERMINAL ARROW ys = list(symbol) DOT
    { (x, ys) }

symbol:
  | nt = NONTERMINAL
    { Grammar.NonTerminal nt }
  | t = TERMINAL
    { Grammar.Terminal t }
