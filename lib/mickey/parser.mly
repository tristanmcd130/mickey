%token FUN
%token LPAREN
%token COMMA
%token RPAREN
%token COLON
%token TINT
%token TUNIT
%token PLUS
%token MINUS
%token SEMICOLON
%token EQUAL
%token LET
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
%token TSTRING
%token <bool> BOOL
%token <int> INT
%token <string> ID
%token <string> STRING
%token EOF
%right SEMICOLON
%right ARROW EQUAL
%left AS
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
| FUN; n = ID; LPAREN; ps = separated_list(COMMA, param); RPAREN; COLON; t = type_; EQUAL; ls = let_*; b = exp	{SFun (n, ps, t, ls, b)}
| VAR; n = ID; COLON; t = type_; EQUAL; v = literal																{Stmt.SVar (n, t, v)}

param: n = ID; COLON; t = type_	{(n, t)}

type_:
| TINT				{TInt}
| TUNIT				{TUnit}
| TBOOL 			{TBool}
| t = type_; TPTR	{TPtr t}
| TSTRING			{Type.TString}

let_: LET; n = ID; COLON; t = type_; IN	{(n, t)}

literal:
| i = INT					{EInt i}
| e = exp; AS; t = type_	{EAs (e, t)}
| b = BOOL					{EBool b}
| LPAREN; RPAREN			{EUnit}
| s = STRING				{EString s}

exp:
| l = literal												{l}
| BREAK; LPAREN; e = exp; RPAREN							{EBreak e}
| n = ID													{EVar n}
| n = ID; LPAREN; a = separated_list(COMMA, exp); RPAREN	{ECall (n, a)}
| AT; n = ID												{EAddrOf n}
| o = unary_op; r = exp										{EUnary (o, r)}
| l = exp; o = binary_op; r = exp							{EBinary (l, o, r)}
| n = ID; EQUAL; v = exp									{ESet (n, v)}
| IF; c = exp; THEN; t = exp; ELSE; e = exp					{EIf (c, t, e)}
| WHILE; c = exp; DO; b = exp								{Exp.EWhile (c, b)}
| LPAREN; e = exp; RPAREN									{e}

%inline unary_op:
| MINUS	{Exp.UNeg}
| NOT	{Exp.UNot}
| BANG	{Exp.UDeref}

%inline binary_op:
| PLUS		{Exp.BAdd}
| MINUS		{Exp.BSub}
| STAR		{Exp.BMul}
| SLASH		{Exp.BDiv}
| EQ		{Exp.BEQ}
| NE		{Exp.BNE}
| GT		{Exp.BGT}
| LT		{Exp.BLT}
| GE		{Exp.BGE}
| LE		{Exp.BLE}
| AND		{Exp.BAnd}
| OR		{Exp.BOr}
| ARROW		{Exp.BPtrSet}
| SEMICOLON	{Exp.BChain}