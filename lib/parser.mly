%token EOF
%token FUN
%token LPAREN
%token COMMA
%token RPAREN
%token COLON
%token TINT
%token TUNIT
%token PLUS
%token MINUS
%token LBRACE
%token SEMICOLON
%token RBRACE
%token EQUAL
%token LOCAL
%token IN
%token BREAK
%token IF
%token THEN
%token ELSE
%token TBOOL
%token <bool> BOOL
%token <int> INT
%token <string> ID
%start <Stmt.t> program
%%

program: ss = stmt*; EOF	{Stmt.SProgram ss}

stmt:
| f = fun_	{f}

fun_: FUN; n = ID; LPAREN; ps = separated_list(COMMA, param); RPAREN; COLON; t = type_; EQUAL; ls = local*; b = exp	{SFun (n, ps, t, ls, b)}

param: n = ID; COLON; t = type_	{(n, t)}

type_:
| TINT	{TInt}
| TUNIT	{TUnit}
| TBOOL {Type.TBool}

local: LOCAL; p = param; IN	{p}

exp:
| i = INT													{EInt i}
| LBRACE; es = separated_list(SEMICOLON, exp); RBRACE		{EBlock es}
| BREAK; LPAREN; e = exp; RPAREN							{EBreak e}
| n = ID; LPAREN; a = separated_list(COMMA, exp); RPAREN	{ECall (n, a)}
| n = ID													{EVar n}
| o = unary_op; r = exp										{EUnary (o, r)}
| l = exp; o = binary_op; r = exp							{EBinary (l, o, r)}
| n = ID; EQUAL; v = exp									{ESet (n, v)}
| b = BOOL													{EBool b}
| LPAREN; e = exp; RPAREN									{e}

%inline unary_op:
| MINUS	{Exp.UNeg}

%inline binary_op:
| PLUS	{Exp.BAdd}
| MINUS	{Exp.BSub}