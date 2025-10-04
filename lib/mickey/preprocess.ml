let rec preprocess = function
| Stmt.SProgram ss -> Stmt.SProgram (List.map preprocess ss)
| SImport f -> In_channel.with_open_text f (fun x -> Lexing.from_channel x |> Parser.program Lexer.read |> preprocess)
| s -> s