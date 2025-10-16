open OUnit2
open Common
open Mickey
open Linker
open Simulator

let prelude_obj = In_channel.with_open_bin "../../prelude.obj" In_channel.input_all |> Bytes.of_string |> Object.of_bytes
let stdlib_obj = In_channel.with_open_bin "../../stdlib.obj" In_channel.input_all |> Bytes.of_string |> Object.of_bytes
let heap_obj = In_channel.with_open_bin "../../heap.obj" In_channel.input_all |> Bytes.of_string |> Object.of_bytes
let run ?(read_callbacks = []) ?(write_callbacks = []) code =
  let ast = Lexing.from_string code |> Parser.program Lexer.read in
  let ast = Stmt.SProgram [SImport "../../stdlib.mks"; ast] |> Preprocess.preprocess in
  let type_defs = Hashtbl.create 10 in
  let type_env = Env.create None [] in
  let typed_ast = Annotate.annotate type_defs type_env ast in
  let program = Program.create () in
  let env = Env.create None [] in
  Compile.compile program type_defs env typed_ast;
  let obj = Program.to_string program |> Lexing.from_string |> Assembler.Parser.program Assembler.Lexer.read |> Assembler.Assemble.assemble in
  let state = State.create ~read_callbacks ~write_callbacks (Link.link [prelude_obj; stdlib_obj; obj; heap_obj]) in
  State.run state;
  state
let make_test value code _ =
  let state = run code in
  assert_equal value (if state.ac > 32767 then state.ac - 65536 else state.ac) ~printer: (fun x -> Printf.sprintf "%d" x)
let make_error_test msg code _ =
  assert_raises (Failure msg) (fun _ -> run code)
let make_output_test output code _ =
  let xmtr_on = ref false in
  let buffer = Buffer.create 10 in
  let _ = run ~write_callbacks: [
    (4094, fun _ value ->
      if !xmtr_on then
        value |> Char.chr |> Buffer.add_char buffer;
      value);
    (4095, fun _ value ->
      xmtr_on := value land 8 = 8;
      value);
  ] code in
  assert_equal output (Buffer.contents buffer) ~printer: (fun x -> x)
let tests = "mickey tests" >::: [
  "int" >:: make_test 5 "fun main(): int {5}";
  "int bigger than 4095" >:: make_test 10001 "fun main(): int {10001}";
  "negative int" >:: make_test (-5) "fun main(): int {-5}";
  "fun" >:: make_test 6 "fun f(x: int): int {x}\nfun main(): int {f(6)}";
  "main type" >:: make_error_test "main function must have type () -> int" "fun main(): unit {()}";
  "type mismatch" >:: make_error_test "Expected a value of type int, but received one of type unit" "fun main(): int {()}";
  "arity checking" >:: make_error_test "Function f expected 2 arguments, but received 0" "fun f(x: int, y: int): int {x}\nfun main(): int {f()}";
  "calling nonfunction" >:: make_error_test "x is not a function, cannot be called" "fun main(): int {var x: int = 4; x(6)}";
  "negation" >:: make_test 5 "fun main(): int {----5}";
  "not" >:: make_test 1 "fun main(): int {(!false) as int}";
  "add" >:: make_test 24 "fun main(): int {9 + 10 + 5}";
  "subtract" >:: make_test (-1) "fun main(): int {9 - 10}";
  "multiply" >:: make_test 924 "fun main(): int {44 * 21}";
  "multiply negative 1st arg" >:: make_test (-45) "fun main(): int {-9 * 5}";
  "multiply negative 2nd arg" >:: make_test (-903) "fun main(): int {43 * -21}";
  "divide" >:: make_test 20 "fun main(): int {440 / 21}";
  "divide negative 1st arg" >:: make_test (-20) "fun main(): int {-440 / 21}";
  "divide negative 2nd arg" >:: make_test (-20) "fun main(): int {440 / -21}";
  "divide negative both args" >:: make_test 20 "fun main(): int {-440 / -21}";
  "modulo" >:: make_test 10 "fun main(): int {466 % 24}";
  "modulo 0" >:: make_test 4 "fun main(): int {4 % 24}";
  "add/mul precedence" >:: make_test 7 "fun main(): int {1 + 2 * 3}";
  "parentheses" >:: make_test 9 "fun main(): int {(1 + 2) * 3}";
  "equal" >:: make_test 1 "fun main(): int {(1 == 1) as int}";
  "equal polymorphism" >:: make_test 0 "fun main(): int {(true == false) as int}";
  "not equal" >:: make_test 0 "fun main(): int {(1 != 1) as int}";
  "greater than" >:: make_test 0 "fun main(): int {(1 > 2) as int}";
  "less than" >:: make_test 1 "fun main(): int {(-3 < -2) as int}";
  "greater than or equal" >:: make_test 1 "fun main(): int {(1 >= -2) as int}";
  "less than or equal" >:: make_test 1 "fun main(): int {(1 <= 2) as int}";
  "mul/compare precedence" >:: make_test 1 "fun main(): int {(5 * 2 >= 5) as int}";
  "and" >:: make_test 1 "fun main(): int {(true & true) as int}";
  "or" >:: make_test 0 "fun main(): int {(false | false) as int}";
  "compare/logic precedence" >:: make_test 1 "fun main(): int {(2 > 1 | 2 < 1) as int}";
  "locals" >:: make_test 51 "fun main(): int {var x: int = 51; x}";
  "globals" >:: make_test 6 "var y: int = 6\nfun main(): int {y}";
  "local precedence" >:: make_test 51 "var x: int = 6\nfun main(): int {var x: int = 51; x}";
  "set local" >:: make_test 32 "fun main(): int {var x: int = 51; x = 32; x}";
  "variable can't change type" >:: make_error_test "Expected a value of type int, but received one of type bool" "fun main(): int {var x: int = 51; x = true; x}";
  "set global" >:: make_test 6 "var x: int = 1\nfun f(): unit {x = 5}\nfun main(): int {var y: int = x; f(); y + x}";
  "break" >:: make_test 2 "fun main(): int {break(2); 5}";
  "if" >:: make_test 4 "fun main(): int {if(true) 4 else 6}";
  "if false" >:: make_test 6 "fun main(): int {if(!true) 4 else 6}";
  "if else" >:: make_test 5 "fun main(): int {if(!true) 1 else if(!!false) 2 else if(true) {3; 5} else 4}";
  "while" >:: make_test 4096 "fun main(): int {var x: int = 2; while(x < 4000) x = x * 2; x}";
  "type checking in block" >:: make_error_test "Expected a value of type int, but received one of type unit" "fun f(): unit {()}\nfun main(): int {var x: int = f(); x}";
  "as precedence" >:: make_test 1 "fun main(): int {!false as int}";
  "as type checking" >:: make_error_test "Expected a value of type int, but received one of type bool" "fun main(): int {(true + ()) as int}";
  "if with no else" >:: make_test 1 "fun main(): int {var x: int = 1; if(false) x = 5; x}";
  (* TODO: figure out how to add tests for import *)
  "sig" >:: make_error_test "Cannot redefine main inconsistently with its previous definition" "sig main(int): unit\nfun main(): int {-5}";
  "sig variables" >:: make_test 5 "sig x: int\nvar x: int = 5\nfun main(): int {x}";
  "double free" >:: make_output_test "Error: Block already freed\n" "fun main(): int {var x: int ptr = alloc(1); free(x); free(x); 5}";
  "index" >:: make_test (Char.code 'b') "var x: char ptr = \"goodbye cruel world\"\nfun main(): int {x[4] as int}";
  "index set" >:: make_test (Char.code 'c') "var x: char ptr = \"goodbye cruel world\"\nfun main(): int {x[4] = 'c'; x[4] as int}";
  "index set nonpointer" >:: make_error_test "Not a pointer, cannot be indexed" "fun main(): int {main[5] = 3; 0}";
  "str_length" >:: make_test 13 "fun main(): int {str_length(\"Hello, world!\")}";
  "else precedence" >:: make_test 1 "fun main(): int {if(true) 1 else 2 + 3}";
  "custom type" >:: make_test 5 "type t = int\nfun main(): int {var x: t = 5; x}";
  (* "custom type mismatch" >:: make_error_test "Expected a value of type t, but received one of type bool" "type t = int\nfun main(): int {var x: t = true; x}"; record types get nominal typing, others get structural so we don't want this test *)
  "struct not enough fields" >:: make_error_test "list was not given all necessary fields: next" "type list = {value: int, next: list}\nvar empty: list = 0 as list\nfun main(): int {var x: list = list{value = 7}; 5}";
  "struct undefined fields" >:: make_error_test "list was given undefined fields: a" "type list = {value: int, next: list}\nvar empty: list = 0 as list\nfun main(): int {var x: list = list{a = true, value = 7, next = empty}; 5}";
  "struct dot" >:: make_test 7 "type list = {value: int, next: list}\nvar empty: list = 0 as list\nfun main(): int {var x: list = list{value = 7, next = list{value = 6, next = empty}}; x.value}";
  "nested struct dot" >:: make_test 2 "type list = {value: int, next: list}\nvar empty: list = 0 as list\nfun main(): int {var x: list = list{value = 7, next = list{value = 6, next = list{value = 2, next = empty}}}; x.next.next.value}";
  "int_to_str" >:: make_output_test "0\n1\n-1\n10000\n-12345\n32767\n-32767\n0\n" "fun main(): int {println(int_to_str(0)); println(int_to_str(1)); println(int_to_str(-1)); println(int_to_str(10000)); println(int_to_str(-12345)); println(int_to_str(32767)); println(int_to_str(-32767)); println(int_to_str(0)); 0}";
  "println" >:: make_output_test "Hello, world!\n" "fun main(): int {println(\"Hello, world!\"); 0}";
  "global unit" >:: make_test 0 "var x: unit = ()\nfun main(): int {x as int}";
  "global char" >:: make_test 115 "var x: char = 's'\nfun main(): int {x as int}";
  "global bool" >:: make_test 1 "var x: bool = true\nfun main(): int {x as int}";
  "ptr literal" >:: make_test 8 "var x: int ptr = [1, 9, 8, 4, 0]\nfun main(): int {x[2]}";
  "address of" >:: make_test 4 "var x: int ptr = [1, 9, 8, 4, 0]\nfun main(): int {(@x)[0][3]}";
  "address of struct field" >:: make_test 7 "type list = {value: int, next: list}\nvar empty: list = 0 as list\nfun main(): int {var x: list = list{value = 7, next = list{value = 6, next = list{value = 2, next = empty}}}; (@x.value)[0]}";
  "address of literal" >:: make_error_test "Not an lval, has no address" "fun main(): int {@5 as int}";
  "struct dot assign" >:: make_test 99 "type list = {value: int, next: list}\nvar empty: list = 0 as list\nfun main(): int {var x: list = list{value = 7, next = list{value = 6, next = list{value = 2, next = empty}}}; x.next.value = 99; x.next.value}";
]
let _ = run_test_tt_main tests