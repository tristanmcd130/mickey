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
%token STAR
%token SLASH
%token EQ
%token NE
%token GT
%token LT
%token GE
%token LE
%token AND
%token OR
%token NOT
%token VAR
%token WHILE
%token DO
%token TPTR
%token BANG
%token ARROW
%token AS
%token AT
%token <bool> BOOL
%token <int> INT
%token <string> ID
%right ARROW
%left OR
%left AND
%left EQ NE LT GT LE GE
%left PLUS MINUS
%left STAR SLASH
%nonassoc NOT BANG
%start <Stmt.t> program
%%

program: ss = stmt*; EOF	{Stmt.SProgram ss}

stmt:
| f = fun_	{f}
| v = var	{v}

fun_: FUN; n = ID; LPAREN; ps = separated_list(COMMA, param); RPAREN; COLON; t = type_; EQUAL; ls = local*; b = exp	{SFun (n, ps, t, ls, b)}

var: VAR; n = ID; COLON; t = type_	{Stmt.SVar (n, t)}

param: n = ID; COLON; t = type_	{(n, t)}

type_:
| TINT				{TInt}
| TUNIT				{TUnit}
| TBOOL 			{TBool}
| t = type_; TPTR	{Type.TPtr t}

local: LOCAL; p = param; IN	{p}

exp:
| i = INT													{EInt i}
| LBRACE; es = separated_list(SEMICOLON, exp); RBRACE		{EBlock es}
| BREAK; LPAREN; e = exp; RPAREN							{EBreak e}
| n = ID													{EVar n}
| n = ID; LPAREN; a = separated_list(COMMA, exp); RPAREN	{ECall (n, a)}
| AT; n = ID												{EAt n}
| o = unary_op; r = exp										{EUnary (o, r)}
| l = exp; o = binary_op; r = exp							{EBinary (l, o, r)}
| e = exp; AS; t = type_									{EAs (e, t)}
| n = ID; EQUAL; v = exp									{ESet (n, v)}
| b = BOOL													{EBool b}
| IF; c = exp; THEN; t = exp; ELSE; e = exp					{EIf (c, t, e)}
| WHILE; c = exp; DO; b = exp								{Exp.EWhile (c, b)}
| LPAREN; e = exp; RPAREN									{e}

%inline unary_op:
| MINUS	{Exp.UNeg}
| NOT	{Exp.UNot}
| BANG	{Exp.UDeref}

%inline binary_op:
| PLUS	{Exp.BAdd}
| MINUS	{Exp.BSub}
| STAR	{Exp.BMul}
| SLASH	{Exp.BDiv}
| EQ	{Exp.BEQ}
| NE	{Exp.BNE}
| GT	{Exp.BGT}
| LT	{Exp.BLT}
| GE	{Exp.BGE}
| LE	{Exp.BLE}
| AND	{Exp.BAnd}
| OR	{Exp.BOr}
| ARROW	{Exp.BPtrSet}