{
	open Lexing
	open Parser
}

let ws = [' ' '\t']+
let id = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let int = '-'? ['0'-'9']+

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
| int		{INT (lexeme lexbuf |> int_of_string)}
| '+'		{PLUS}
| '-'		{MINUS}
| ';'		{SEMICOLON}
| '='		{EQUAL}
| "break"	{BREAK}
| "if"		{IF}
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
| "ptr"		{TPTR}
| '!'		{BANG}
| "as"		{AS}
| '@'		{AT}
| "string"	{TSTRING}
| '{'		{LBRACE}
| '}'		{RBRACE}
| "import"	{IMPORT}
| "sig"		{SIG}
| id		{ID (lexeme lexbuf)}
| '#'		{skip_comment lexbuf}
| '"'		{read_string (Buffer.create 10) lexbuf}
| _			{failwith ("Unexpected character: " ^ lexeme lexbuf)}
| eof		{EOF}
and skip_comment = parse
| '\n'	{new_line lexbuf; read lexbuf}
| eof	{EOF}
| _		{skip_comment lexbuf}
and read_string buf = parse
| '"'		{STRING (Buffer.contents buf)}
| "\\\""	{Buffer.add_string buf "\\\""; read_string buf lexbuf}
| _			{Buffer.add_string buf (lexeme lexbuf); read_string buf lexbuf}
| eof		{failwith "Unterminated string"}