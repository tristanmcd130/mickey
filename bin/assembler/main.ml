open Common
open Assembler

let () =
  if Array.length Sys.argv <> 3 then
    (prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <input> <output>");
    exit 1);
  let object' = In_channel.with_open_bin Sys.argv.(1) (fun x -> Lexing.from_channel x |> Parser.program Lexer.read |> Assemble.assemble) in
  Out_channel.with_open_bin Sys.argv.(2) (fun x -> output_bytes x (Object.to_bytes object'))