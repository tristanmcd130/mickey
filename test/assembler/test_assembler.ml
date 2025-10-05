open OUnit2
open Common
open Assembler

let make_test labels relocations code asm _ =
  assert_equal ({
    labels = labels |> List.to_seq |> Hashtbl.of_seq;
    relocations = relocations |> List.to_seq |> Hashtbl.of_seq;
    code =
      let buffer = Buffer.create 10 in
      List.iter (Buffer.add_uint16_be buffer) code;
      Buffer.to_bytes buffer
  }: Object.t) (asm |> Lexing.from_string |> Parser.program Lexer.read |> Assemble.assemble) ~printer: (fun (x: Object.t) -> x.code |> Bytes.escaped |> Bytes.to_string)
let make_error_test msg asm _ =
  assert_raises (Failure msg) (fun _ -> asm |> Lexing.from_string |> Parser.program Lexer.read |> Assemble.assemble)
let tests = "assembler tests" >::: [
  "empty" >:: make_test [] [] [] "";
  "lodd" >:: make_test [] [] [0x000a] "lodd 10";
  "undefined label" >:: make_test [] [(0, "a")] [0x0000] "lodd a";
  "defined label" >:: make_test [("a", 1)] [(0, "a")] [0x0000] "lodd a\na:";
  "stod" >:: make_test [] [] [0x100a] "stod 10";
  "multiple instructions" >:: make_test [] [] [0x000a; 0x100a] "lodd 10\nstod 10";
  "addd" >:: make_test [] [] [0x200a] "addd 10";
  "subd" >:: make_test [] [] [0x300a] "subd 10";
  "jpos" >:: make_test [] [] [0x400a] "jpos 10";
  "jzer" >:: make_test [] [] [0x500a] "jzer 10";
  "jump" >:: make_test [] [] [0x600a] "jump 10";
  "loco" >:: make_test [] [] [0x700b] "loco 11";
  "lodl" >:: make_test [] [] [0x800c] "lodl 12";
  "stol" >:: make_test [] [] [0x900c] "stol 12";
  "addl" >:: make_test [] [] [0xa00c] "addl 12";
  "subl" >:: make_test [] [] [0xb00c] "subl 12";
  "local instruction invalid args" >:: make_error_test "Argument must be in 0-255" "lodl 312";
  "jneg" >:: make_test [] [] [0xc014] "jneg 20";
  "jnze" >:: make_test [] [] [0xd014] "jnze 20";
  "call" >:: make_test [] [] [0xe014] "call 20";
  "pshi" >:: make_test [] [] [0xf000] "pshi";
  "popi" >:: make_test [] [] [0xf200] "popi";
  "push" >:: make_test [] [] [0xf400] "push";
  "pop" >:: make_test [] [] [0xf600] "pop";
  "retn" >:: make_test [] [] [0xf800] "retn";
  "swap" >:: make_test [] [] [0xfa00] "swap";
  "insp" >:: make_test [] [] [0xfc0e] "insp 14";
  "desp" >:: make_test [] [] [0xfe0e] "desp 14";
  "halt" >:: make_test [] [] [0xff00] "halt";
  "consecutive labels" >:: make_test [("a", 1); ("b", 1)] [] [0x0000] "lodd 0\na:\nb:";
  "int" >:: make_test [] [] [0x0064; 0x00c8] "100 200";
  "string" >:: make_test [] [] ("hello world\n\x00" |> String.to_seq |> Seq.map Char.code |> List.of_seq) "\"hello world\\n\"";
  "label pointing to int" >:: make_test [("a", 0)] [] [0x000b] "a: 11";
  "label redefinition" >:: make_error_test "Label a already defined at 0" "a: 10 a: 11";
  "char" >:: make_test [] [] [Char.code 't'] "'t'";
  "escaped char" >:: make_test [] [] [Char.code '\t'] "'\\t'";
  "backslash" >:: make_test [] [] [Char.code '\\'] "'\\\\'";
]
let _ = run_test_tt_main tests