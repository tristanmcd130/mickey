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
| ':'		{COLON}
| "lodd"	{LODD}
| "stod"	{STOD}
| "addd"	{ADDD}
| "subd"	{SUBD}
| "jpos"	{JPOS}
| "jzer"	{JZER}
| "jump"	{JUMP}
| "loco"	{LOCO}
| "lodl"	{LODL}
| "stol"	{STOL}
| "addl"	{ADDL}
| "subl"	{SUBL}
| "jneg"	{JNEG}
| "jnze"	{JNZE}
| "call"	{CALL}
| "pshi"	{PSHI}
| "popi"	{POPI}
| "push"	{PUSH}
| "pop"		{POP}
| "retn"	{RETN}
| "swap"	{SWAP}
| "insp"	{INSP}
| "desp"	{DESP}
| "halt"	{HALT}
| id		{LABEL (lexeme lexbuf)}
| int		{INT (lexeme lexbuf |> int_of_string)}
| '"'		{read_string (Buffer.create 10) lexbuf}
| ';'		{skip_comment lexbuf}
| _			{failwith ("Unexpected character: " ^ lexeme lexbuf)}
| eof		{EOF}
and skip_comment = parse
| '\n'	{new_line lexbuf; read lexbuf}
| eof	{EOF}
| _		{skip_comment lexbuf}
and read_string buf = parse
| '"'			{STRING (Buffer.contents buf)}
| "\\\\"		{Buffer.add_char buf '\\'; read_string buf lexbuf}
| "\\n"			{Buffer.add_char buf '\n'; read_string buf lexbuf}
| "\\t"			{Buffer.add_char buf '\t'; read_string buf lexbuf}
| [^'"' '\\']	{Buffer.add_string buf (lexeme lexbuf); read_string buf lexbuf}
| _				{failwith ("Unexpected string character: " ^ lexeme lexbuf)}
| eof			{failwith "Unterminated string"}