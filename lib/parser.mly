%token EOF
%token FUN
%token LPAREN
%token COMMA
%token RPAREN
%token COLON
%token TINT
%token TUNIT
%token LBRACE
%token RBRACE
%token <int> INT
%token <string> ID
%start <Ast.t> program
%%

program: fs = fun_*; EOF	{Ast.AProgram fs}

fun_: FUN; n = ID; LPAREN; ps = separated_list(COMMA, param); RPAREN; COLON; t = type_; b = exp	{AFun (n, ps, t, b)}

param: n = ID; COLON; t = type_	{(n, t)}

type_:
| TINT	{TInt}
| TUNIT	{Type.TUnit}

exp:
| i = INT					{AInt i}
| LBRACE; es = exp*; RBRACE {ABlock es}