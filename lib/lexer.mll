{
	open Lexing
	open Parser
}

let ws = [' ' '\t']+
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let int = ['0'-'9']+

rule read = parse
| ws		{read lexbuf}
| '\n'		{new_line lexbuf; read lexbuf}
| "fun"		{FUN}
| '('		{LPAREN}
| ','		{COMMA}
| ')'		{RPAREN}
| ':'		{COLON}
| "int"		{TINT}
| "unit"	{TUNIT}
| '+'		{PLUS}
| '-'		{MINUS}
| '{'		{LBRACE}
| ';'		{SEMICOLON}
| '}'		{RBRACE}
| '='		{EQUAL}
| "local"	{LOCAL}
| "in"		{IN}
| "break"	{BREAK}
| "if"		{IF}
| "then"	{THEN}
| "else"	{ELSE}
| "true"	{BOOL true}
| "false"	{BOOL false}
| "bool"	{TBOOL}
| '*'		{STAR}
| '/'		{SLASH}
| "=="		{EQ}
| "!="		{NE}
| '>'		{GT}
| '<'		{LT}
| ">="		{GE}
| "<="		{LE}
| "and"		{AND}
| "or"		{OR}
| "not"		{NOT}
| "var"		{VAR}
| "while"	{WHILE}
| "do"		{DO}
| int		{INT (lexeme lexbuf |> int_of_string)}
| id		{ID (lexeme lexbuf)}
| '#'		{skip_comment lexbuf}
| _			{failwith ("Unexpected character: " ^ lexeme lexbuf)}
| eof		{EOF}
and skip_comment = parse
| '\n'	{new_line lexbuf; read lexbuf}
| eof	{EOF}
| _		{skip_comment lexbuf}