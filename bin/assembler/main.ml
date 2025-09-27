open Common
open Assembler

let () =
  if Array.length Sys.argv <> 3 then
    (prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <input> <output>");
    exit 1)
  else
    (let object' = open_in Sys.argv.(1) |> Lexing.from_channel |> Parser.program Lexer.read |> Assemble.assemble in
    let out = open_out_bin Sys.argv.(2) in
    output_bytes out (Object.to_bytes object');
    close_out out)