%token <bool> BOOL
%token <int> NUMBER
%token <string> STRING
%token EQUAL
%token ARROW
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token PERCENT
%token LT
%token LE
%token EQ
%token NE
%token GT
%token GE
%token AND
%token OR
%token NOT
%token AT
%token IF
%token THEN
%token ELSE
%token END
%token WHILE
%token DO
%token FUN
%token LPAREN
%token COMMA
%token RPAREN
%token COLON
%token LOCALS
%token BEGIN
%token RETURN
%token PTR
%token GLOBALS
%token TVOID
%token TBOOL
%token TINT
%token IMPORT
%token ASM
%token AS
%token <string> ID
%token EOF
%left OR
%left AND
%left LT LE EQ NE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc NOT AT
%start <Stmt.t> prog
%%

prog: is = import*; gs = globals; fs = fun_def*; EOF	{Stmt.SBlock (gs @ fs)}

import: IMPORT; f = STRING	{Stmt.SImport f}

globals:
	| 												{[]}
	| GLOBALS; gs = param*; BEGIN; b = block; END	{[SGlobals (gs, b)]}

fun_def: FUN; n = ID; LPAREN; ps = separated_list(COMMA, param); RPAREN; COLON; t = type_; ls = locals; BEGIN; b = block; END	{SFun (n, ps, t, ls, b)}

param: n = ID; COLON; t = type_	{(n, t)}

type_:
	| TVOID				{TVoid}
	| TBOOL				{TBool}
	| TINT				{TInt}
	| t = type_; PTR	{Type.TPtr t}

locals:
	|						{[]}
	| LOCALS; ls = param*	{ls}

block: b = stmt*	{SBlock b}

stmt:
	| n = ID; EQUAL; v = exp									{SAssign (n, v)}
	| e = exp; ARROW; v = exp									{SPtrAssign (e, v)}
	| n = ID; LPAREN; a = separated_list(COMMA, exp); RPAREN	{SCall (n, a)}
	| IF; c = exp; THEN; t = block; ELSE; e = block; END		{SIf (c, t, e)}
	| WHILE; c = exp; DO; b = block; END						{SWhile (c, b)}
	| RETURN; e = exp?											{SReturn e}
	| ASM; s = STRING											{Stmt.SAsm s}

exp:
	| b = BOOL													{EBool b}
	| n = NUMBER												{EInt n}
	| s = STRING												{EString s}
	| v = ID													{EVar v}
	| n = ID; LPAREN; a = separated_list(COMMA, exp); RPAREN	{ECall (n, a)}
	| AT; e = exp												{EAt e}
	| o = unary_op; e = exp										{ECall (o, [e])}
	| e1 = exp; o = binary_op; e2 = exp							{ECall (o, [e1; e2])}
	| e = exp; AS; t = type_									{EAs (e, t)}
	| LPAREN; e = exp; RPAREN									{e}

%inline unary_op:
	| NOT	{"not"}
	| MINUS	{"neg"}

%inline binary_op:
	| OR		{"or"}
	| AND		{"and"}
	| LT		{"lt"}
	| LE		{"le"}
	| EQ		{"eq"}
	| NE		{"ne"}
	| GT		{"gt"}
	| GE		{"ge"}
	| PLUS		{"add"}
	| MINUS		{"sub"}
	| STAR		{"mul"}
	| SLASH		{"div"}
	| PERCENT	{"mod"}