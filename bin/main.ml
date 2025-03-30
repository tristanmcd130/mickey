open Mickey
open Lexing

let () =
  match Sys.argv with
  | [|_; f|] -> 
    let ast = In_channel.open_text f |> from_channel |> Parser.prog Lexer.read in
    (try
      Type_check.type_check ast (Type_check.process_defs ast @ [
        ("not", TFun ([TBool], TBool));
        ("neg", TFun ([TInt], TInt));
        ("or", TFun ([TBool; TBool], TBool));
        ("and", TFun ([TBool; TBool], TBool));
        ("lt", TFun ([TInt; TInt], TBool));
        ("le", TFun ([TInt; TInt], TBool));
        ("gt", TFun ([TInt; TInt], TBool));
        ("ge", TFun ([TInt; TInt], TBool));
        ("add", TFun ([TInt; TInt], TInt));
        ("sub", TFun ([TInt; TInt], TInt));
        ("mul", TFun ([TInt; TInt], TInt));
        ("div", TFun ([TInt; TInt], TInt));
        ("mod", TFun ([TInt; TInt], TInt));
      ]) |> ignore;
      Compile.compile ast [] |> print_endline
    with
    | e -> print_endline ("Error: " ^ Printexc.to_string e))
  |_ -> print_endline ("Usage: " ^ Sys.argv.(0) ^ " [file]")