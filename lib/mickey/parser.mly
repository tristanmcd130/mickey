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
%token PERCENT
%token EQ
%token NE
%token GT
%token LT
%token GE
%token LE
%token AND
%token PIPE
%token BANG
%token VAR
%token WHILE
%token TPTR
%token AS
%token LBRACE
%token RBRACE
%token IMPORT
%token SIG
%token LBRACKET
%token RBRACKET
%token TCHAR
%token TYPE
%token DOT
%token AT
%token <bool> BOOL
%token <int> INT
%token <string> ID
%token <string> STRING
%token <string> CHAR
%token EOF
%right EQUAL
%left PIPE
%left AND
%left EQ NE LT GT LE GE
%left PLUS MINUS PERCENT
%left STAR SLASH
%left AS
%nonassoc BANG AT
%nonassoc LBRACKET
%nonassoc DOT
%start <unit Stmt.t> program
%%

program: ss = stmt*; EOF	{Stmt.SProgram ss}

stmt:
| FUN; n = ID; LPAREN; ps = separated_list(COMMA, param); RPAREN; COLON; t = type_; LBRACE; ls = local*; b = separated_nonempty_list(SEMICOLON, exp); RBRACE	{SFun (n, ps, t, ls, (EBlock b, ()))}
| VAR; n = ID; COLON; t = type_; EQUAL; v = literal																													{SVar (n, t, v)}
| SIG; n = ID; LPAREN; ps = separated_list(COMMA, type_); RPAREN; COLON; t = type_																					{SSig (n, TArrow (ps, t))}
| SIG; n = ID; COLON; t = type_																																		{SSig (n, t)}
| IMPORT; s = STRING																																				{SImport s}
| TYPE; n = ID																																						{STypeDef (n, TOpaque)}
| TYPE; n = ID; EQUAL; t = top_type																																	{Stmt.STypeDef (n, t)}

param: n = ID; COLON; t = type_	{(n, t)}

local: VAR; n = ID; COLON; t = type_; EQUAL; v = exp; SEMICOLON	{(n, t, v)}

top_type:
| LBRACE; fs = separated_list(COMMA, param); RBRACE	{TStruct fs}
| t = type_											{t}

type_:
| TINT				{TInt}
| TUNIT				{TUnit}
| TBOOL 			{TBool}
| t = type_; TPTR	{TPtr t}
| TCHAR 			{TChar}
| n = ID			{Type.TName n}

literal:
| i = INT														{(EInt i, ())}
| l = literal; AS; t = type_									{(EAs (l, t), ())}
| b = BOOL														{(EBool b, ())}
| LPAREN; RPAREN												{(EUnit, ())}
| s = STRING													{(EString s, ())}
| c = CHAR														{(EChar c, ())}
| LBRACKET; es = separated_list(COMMA, array_literal); RBRACKET	{(Exp.EArray es, ())}

array_literal:
| i = INT							{(EInt i, ())}
| l = array_literal; AS; t = type_	{(EAs (l, t), ())}
| b = BOOL							{(EBool b, ())}
| LPAREN; RPAREN					{(EUnit, ())}
| c = CHAR							{(Exp.EChar c, ())}

exp:
| i = INT														{(EInt i, ())}
| b = BOOL														{(EBool b, ())}
| LPAREN; RPAREN												{(EUnit, ())}
| s = STRING													{(EString s, ())}
| c = CHAR														{(EChar c, ())}
| LBRACE; es = separated_list(SEMICOLON, exp); RBRACE			{(EBlock es, ())}
| BREAK; LPAREN; e = exp; RPAREN								{(EBreak e, ())}
| n = ID; LPAREN; a = separated_list(COMMA, exp); RPAREN		{(ECall (n, a), ())}
| o = unary_op; r = exp											{(EUnary (o, r), ())}
| l = exp; o = binary_op; r = exp								{(EBinary (l, o, r), ())}
| e = exp; AS; t = type_										{(EAs (e, t), ())}
| IF; LPAREN; c = exp; RPAREN; t = exp; e = else_				{(EIf (c, t, e), ())}
| WHILE; LPAREN; c = exp; RPAREN; b = exp						{(EWhile (c, b), ())}
| n = ID; LBRACE; fs = separated_list(COMMA, field); RBRACE		{(EStruct (n, fs), ())}
| LBRACKET; es = separated_list(COMMA, array_literal); RBRACKET	{(EArray es, ())}
| AT; l = exp													{(EAddrOf l, ())}
| e = exp; EQUAL; v = exp										{(ESet (e, v), ())}
| n = ID														{(EVar n, ())}
| l = exp; LBRACKET; i = exp; RBRACKET							{(EIndex (l, i), ())}
| l = exp; DOT; f = ID											{(Exp.EDot (l, f), ())}
| LPAREN; e = exp; RPAREN										{e}

%inline unary_op:
| MINUS	{Exp.UNeg}
| BANG	{Exp.UNot}

%inline binary_op:
| PLUS		{Exp.BAdd}
| MINUS		{Exp.BSub}
| STAR		{Exp.BMul}
| SLASH		{Exp.BDiv}
| PERCENT	{Exp.BMod}
| EQ		{Exp.BEQ}
| NE		{Exp.BNE}
| GT		{Exp.BGT}
| LT		{Exp.BLT}
| GE		{Exp.BGE}
| LE		{Exp.BLE}
| AND		{Exp.BAnd}
| PIPE		{Exp.BOr}

else_:
|					{(EUnit, ())}
| ELSE; e = exp	{e}

field: f = ID; EQUAL; v = exp	{(f, v)}