open Mickey
open Lexing

let () =
  match Sys.argv with
  | [|_; f|] -> 
    let ast = In_channel.open_text f |> from_channel |> Parser.prog Lexer.read in
    (try
      Type_check.type_check ast (Type_check.process_defs ast) |> ignore;
      Compile.compile ast [] |> print_endline
    with
    | e -> print_endline ("Error: " ^ Printexc.to_string e))
  |_ -> print_endline ("Usage: " ^ Sys.argv.(0) ^ " [file]")