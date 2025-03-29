{
	open Lexing
	open Parser
}

let ws = [' ' '\t']+
let number = ['0'-'9']+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
	parse
	| ws		{read lexbuf}
	| '\n'		{new_line lexbuf; read lexbuf}
	| "true"	{BOOL true}
	| "false"	{BOOL false}
	| number	{NUMBER (int_of_string (Lexing.lexeme lexbuf))}
	(* | '"'		{read_string (Buffer.create 16) lexbuf} *)
	| '='		{EQUAL}
	| '+'		{PLUS}
	| '-'		{MINUS}
	| '*'		{STAR}
	| '/'		{SLASH}
	| '%'		{PERCENT}
	| '<'		{LT}
	| "<="		{LE}
	| "=="		{EQ}
	| "!="		{NE}
	| '>'		{GT}
	| ">="		{GE}
	| "and"		{AND}
	| "or"		{OR}
	| "not"		{NOT}
	| "if"		{IF}
	| "then"	{THEN}
	| "else"	{ELSE}
	| "end"		{END}
	| "while"	{WHILE}
	| "do"		{DO}
	| "fun"		{FUN}
	| '('		{LPAREN}
	| ','		{COMMA}
	| ')'		{RPAREN}
	| ':'		{COLON}
	| "locals"	{LOCALS}
	| "begin"	{BEGIN}
	| "return"	{RETURN}
	| "ptr"		{PTR}
	| "globals"	{GLOBALS}
	| "void"	{TVOID}
	| "bool"	{TBOOL}
	| "int"		{TINT}
	| id		{ID (Lexing.lexeme lexbuf)}
	| '#'		{skip_comment lexbuf}
	| eof		{EOF}
	| _			{failwith ("Illegal character: " ^ Lexing.lexeme lexbuf)}
and read_string buf =
	parse
	| '"'			{STRING (Buffer.contents buf)}
	| '\\' '\\'		{Buffer.add_char buf '\\'; read_string buf lexbuf}
	| '\\' 't'		{Buffer.add_char buf '\t'; read_string buf lexbuf}
	| '\\' 'n'		{Buffer.add_char buf '\n'; read_string buf lexbuf}
	| [^ '"' '\\']+	{Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf}
	| eof			{failwith "String not terminated"}
	| _				{failwith ("Illegal character in string: " ^ Lexing.lexeme lexbuf)}
and skip_comment =
	parse
	| '\n'	{new_line lexbuf; read lexbuf}
	| eof	{EOF}
	| _		{skip_comment lexbuf}