open OUnit2
open Simulator

let make_test test program _ =
  let buffer = Buffer.create 10 in
  List.iter (Buffer.add_uint16_be buffer) program;
  let state = State.create (Buffer.to_bytes buffer) in
  State.run state;
  test state
let assert_equal x y = assert_equal x y ~printer: (fun x -> Printf.sprintf "%04x" x)
let tests = "simulator tests" >::: [
  "lodd" >:: make_test (fun state ->
    assert_equal 0x1234 state.ac) [0x0002; 0xff00; 0x1234];
  "stod" >:: make_test (fun state ->
    assert_equal 0x0000 (State.read state 0x0004);
    assert_equal 0x1234 (State.read state 0x0005)) [0x0003; 0x1005; 0xff00; 0x1234; 0x0000; 0x0000];
  "addd" >:: make_test (fun state ->
    assert_equal 0x1235 state.ac) [0x0003; 0x2004; 0xff00; 0x1234; 0x0001];
  "addd overflow" >:: make_test (fun state ->
    assert_equal 0x0068 state.ac) [0x0003; 0x2004; 0xff00; 0x8001; 0x8067];
  "subd" >:: make_test (fun state ->
    assert_equal 0x1233 state.ac) [0x0003; 0x3004; 0xff00; 0x1234; 0x0001];
  "subd underflow" >:: make_test (fun state ->
    assert_equal 0xedcd state.ac) [0x0003; 0x3004; 0xff00; 0x0001; 0x1234];
  "jpos true" >:: make_test (fun state ->
    assert_equal 0x0004 state.pc) [0x0004; 0x4003; 0xff00; 0xff00; 0x7fff];
  "jpos false" >:: make_test (fun state ->
    assert_equal 0x0003 state.pc) [0x0004; 0x4003; 0xff00; 0xff00; 0x8001];
  "jzer true" >:: make_test (fun state ->
    assert_equal 0x0004 state.pc) [0x0004; 0x5003; 0xff00; 0xff00; 0x0000];
  "jzer false" >:: make_test (fun state ->
    assert_equal 0x0003 state.pc) [0x0004; 0x5003; 0xff00; 0xff00; 0x0001];
  "jump" >:: make_test (fun state ->
    assert_equal 0x0003 state.pc) [0x6002; 0xff00; 0xff00; 0xff00];
  "loco" >:: make_test (fun state ->
    assert_equal 0x0123 state.ac) [0x7123; 0xff00];
  "lodl" >:: make_test (fun state ->
    assert_equal 0x0456 state.ac) [0x7456; 0xf400; 0x7123; 0xf400; 0x8001; 0xff00];
  "stol" >:: make_test (fun state ->
    assert_equal 0x0123 (State.read state (state.sp + 1))) [0x7456; 0xf400; 0x7123; 0x9001; 0xff00];
  "addl" >:: make_test (fun state ->
    assert_equal 0x0579 state.ac) [0x7456; 0xf400; 0x7123; 0xa000; 0xff00];
  "subl" >:: make_test (fun state ->
    assert_equal 0xfccd state.ac) [0x7456; 0xf400; 0x7123; 0xb000; 0xff00];
  "jneg true" >:: make_test (fun state ->
    assert_equal 0x0004 state.pc) [0x0004; 0xc003; 0xff00; 0xff00; 0x8888];
  "jneg false" >:: make_test (fun state ->
    assert_equal 0x0003 state.pc) [0x0004; 0xc003; 0xff00; 0xff00; 0x0000];
  "jnze true" >:: make_test (fun state ->
    assert_equal 0x0004 state.pc) [0x0004; 0xd003; 0xff00; 0xff00; 0x0001];
  "jnze false" >:: make_test (fun state ->
    assert_equal 0x0003 state.pc) [0x0004; 0xd003; 0xff00; 0xff00; 0x0000];
  "call" >:: make_test (fun state ->
    assert_equal 0x0003 state.pc;
    assert_equal 0x0f7f state.sp;
    assert_equal 0x0001 (State.read state state.sp)) [0xe002; 0xff00; 0xff00];
  "pshi" >:: make_test (fun state ->
    assert_equal 0x1234 (State.read state state.sp);
    assert_equal 0x0f7f state.sp) [0x7003; 0xf000; 0xff00; 0x1234];
  "popi" >:: make_test (fun state ->
    assert_equal 0x0670 (State.read state 0x0005);
    assert_equal 0x0f80 state.sp) [0x7670; 0xf400; 0x7005; 0xf200; 0xff00; 0x1234];
  "push" >:: make_test (fun state ->
    assert_equal 0x0670 (State.read state state.sp);
    assert_equal 0x0f7f state.sp) [0x7670; 0xf400; 0xff00];
  "pop" >:: make_test (fun state ->
    assert_equal 0x0670 state.ac;
    assert_equal 0x0f80 state.sp) [0x7670; 0xf400; 0xf600; 0xff00];
  "retn" >:: make_test (fun state ->
    assert_equal 0x0002 state.pc;
    assert_equal 0x0f80 state.sp) [0xe002; 0xff00; 0xf800; 0xff00];
  "swap" >:: make_test (fun state ->
    assert_equal 0x0f80 state.ac;
    assert_equal 0x0890 state.sp) [0x7890; 0xfa00; 0xff00];
  "insp" >:: make_test (fun state ->
    assert_equal 0x0f81 state.sp) [0xfc01; 0xff00];
  "desp" >:: make_test (fun state ->
    assert_equal 0x0f70 state.sp) [0xfe10; 0xff00];
]
let _ = run_test_tt_main tests