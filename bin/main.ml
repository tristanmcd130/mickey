open Mickey

let () = 
  let ast = stdin |> Lexing.from_channel |> Parser.program Lexer.read in
  let scope = Scope.create None [] in
  Type_check.type_check scope ast;
  Compile.compile ast |> print_endline