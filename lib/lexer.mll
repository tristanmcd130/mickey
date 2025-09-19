{
	open Lexing
	open Parser
}

let ws = [' ' '\t']+
let nl = '\n'
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int = '-'? ['0'-'9']+

rule read = parse
| ws		{read lexbuf}
| nl		{new_line lexbuf; read lexbuf}
| "fun"		{FUN}
| '('		{LPAREN}
| ','		{COMMA}
| ')'		{RPAREN}
| ':'		{COLON}
| "int"		{TINT}
| "unit"	{TUNIT}
| '{'		{LBRACE}
| '}'		{RBRACE}
| int		{INT (lexeme lexbuf |> int_of_string)}
| id		{ID (lexeme lexbuf)}
| _			{failwith ("Unexpected character: " ^ lexeme lexbuf)}
| eof		{EOF}