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
        ("alloc", TFun ([TInt], TPtr TVoid));
        ("free", TFun ([TPtr TVoid], TVoid));
      ]) |> ignore;
      let (global_init, globals, code) = Compile.compile ast [] in
      let output = open_out ((f |> String.split_on_char '.' |> List.hd) ^ ".asm") in
      Printf.fprintf output "%s" global_init;
      Printf.fprintf output "%s\n" (In_channel.open_text "prelude.asm" |> In_channel.input_all);
      Printf.fprintf output "%s" globals;
      Printf.fprintf output "%s" code;
      Printf.fprintf output "\nheap:\n1\n0\n0";
      close_out output
    with
    | e ->
      print_endline ("Error: " ^ Printexc.to_string e);
      exit 1)
  |_ -> print_endline ("Usage: " ^ Sys.argv.(0) ^ " [file]")