open Mickey

let () =
  if Array.length Sys.argv <> 3 then
    (prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <input> <output>");
    exit 1);
  let ast = In_channel.with_open_text Sys.argv.(1) (fun x -> Lexing.from_channel x |> Parser.program Lexer.read) in
  let ast = Stmt.SProgram [SImport "stdlib.mks"; ast] |> Preprocess.preprocess in
  let type_env = Env.create None [] in
  Type_check.type_check type_env ast;
  let program = Program.create () in
  let env = Env.create None [] in
  Compile.compile program env ast;
  Out_channel.with_open_text Sys.argv.(2) (fun x -> Program.to_string program |> output_string x)