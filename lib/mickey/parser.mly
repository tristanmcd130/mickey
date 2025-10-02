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
%token BREAK
%token IF
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
%token TPTR
%token BANG
%token ARROW
%token AS
%token AT
%token TSTRING
%token LBRACE
%token RBRACE
%token <bool> BOOL
%token <int> INT
%token <string> ID
%token <string> STRING
%token EOF
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
| FUN; n = ID; LPAREN; ps = separated_list(COMMA, param); RPAREN; COLON; t = type_; LBRACE; ls = local*; b = separated_nonempty_list(SEMICOLON, exp); RBRACE	{SFun (n, ps, t, ls, EBlock b)}
| VAR; n = ID; COLON; t = type_; EQUAL; v = literal																												{Stmt.SVar (n, t, v)}

param: n = ID; COLON; t = type_	{(n, t)}

local: VAR; n = ID; COLON; t = type_; EQUAL; v = exp; SEMICOLON	{(n, t, v)}

type_:
| TINT				{TInt}
| TUNIT				{TUnit}
| TBOOL 			{TBool}
| t = type_; TPTR	{TPtr t}
| TSTRING			{Type.TString}

literal:
| i = INT						{EInt i}
| l = literal; AS; t = type_	{EAs (l, t)}
| b = BOOL						{EBool b}
| LPAREN; RPAREN				{EUnit}
| s = STRING					{EString s}

exp:
| i = INT													{EInt i}
| b = BOOL													{EBool b}
| LPAREN; RPAREN											{EUnit}
| s = STRING												{EString s}
| LBRACE; es = separated_list(SEMICOLON, exp); RBRACE		{EBlock es}
| BREAK; LPAREN; e = exp; RPAREN							{EBreak e}
| n = ID													{EVar n}
| n = ID; LPAREN; a = separated_list(COMMA, exp); RPAREN	{ECall (n, a)}
| AT; n = ID												{EAddrOf n}
| o = unary_op; r = exp										{EUnary (o, r)}
| l = exp; o = binary_op; r = exp							{EBinary (l, o, r)}
| e = exp; AS; t = type_									{EAs (e, t)}
| n = ID; EQUAL; v = exp									{ESet (n, v)}
| IF; LPAREN; c = exp; RPAREN; t = exp; ELSE; e = exp		{EIf (c, t, e)}
| WHILE; LPAREN; c = exp; RPAREN; b = exp					{Exp.EWhile (c, b)}
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