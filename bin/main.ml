open Mickey

let () = 
  let ast = stdin |> Lexing.from_channel |> Parser.program Lexer.read in
  let type_env = Env.create None [] in
  Type_check.type_check type_env ast;
  let program = Program.create () in
  let env = Env.create None [] in
  Compile.compile program env ast;
  Program.to_string program |> print_endline