open OUnit2
open Simulator

let make_test program test _ =
  let buffer = Buffer.create 10 in
  List.iter (Buffer.add_uint16_be buffer) program;
  let state = State.create (Buffer.to_bytes buffer) in
  State.run state;
  test state
let assert_equal x y = assert_equal x y ~printer: (fun x -> Printf.sprintf "%04x" x)
let tests = "simulator tests" >::: [
  "lodd" >:: make_test [0x0002; 0xff00; 0x1234] (fun state ->
    assert_equal 0x1234 state.ac);
  "stod" >:: make_test [0x0003; 0x1005; 0xff00; 0x1234; 0x0000; 0x0000] (fun state ->
    assert_equal 0x0000 (State.read state 0x0004);
    assert_equal 0x1234 (State.read state 0x0005));
  "addd" >:: make_test [0x0003; 0x2004; 0xff00; 0x1234; 0x0001] (fun state ->
    assert_equal 0x1235 state.ac);
  "addd overflow" >:: make_test [0x0003; 0x2004; 0xff00; 0x8001; 0x8067] (fun state ->
    assert_equal 0x0068 state.ac);
  "subd" >:: make_test [0x0003; 0x3004; 0xff00; 0x1234; 0x0001] (fun state ->
    assert_equal 0x1233 state.ac);
  "subd underflow" >:: make_test [0x0003; 0x3004; 0xff00; 0x0001; 0x1234] (fun state ->
    assert_equal 0xedcd state.ac);
  "jpos true" >:: make_test [0x0004; 0x4003; 0xff00; 0xff00; 0x7fff] (fun state ->
    assert_equal 0x0004 state.pc);
  "jpos false" >:: make_test [0x0004; 0x4003; 0xff00; 0xff00; 0x8001] (fun state ->
    assert_equal 0x0003 state.pc);
  "jzer true" >:: make_test [0x0004; 0x5003; 0xff00; 0xff00; 0x0000] (fun state ->
    assert_equal 0x0004 state.pc);
  "jzer false" >:: make_test [0x0004; 0x5003; 0xff00; 0xff00; 0x0001] (fun state ->
    assert_equal 0x0003 state.pc);
  "jump" >:: make_test [0x6002; 0xff00; 0xff00; 0xff00] (fun state ->
    assert_equal 0x0003 state.pc);
  "loco" >:: make_test [0x7123; 0xff00] (fun state ->
    assert_equal 0x0123 state.ac);
  "lodl" >:: make_test [0x7456; 0xf400; 0x7123; 0xf400; 0x8001; 0xff00] (fun state ->
    assert_equal 0x0456 state.ac);
  "stol" >:: make_test [0x7456; 0xf400; 0x7123; 0x9001; 0xff00] (fun state ->
    assert_equal 0x0123 (State.read state (state.sp + 1)));
  "addl" >:: make_test [0x7456; 0xf400; 0x7123; 0xa000; 0xff00] (fun state ->
    assert_equal 0x0579 state.ac);
  "subl" >:: make_test [0x7456; 0xf400; 0x7123; 0xb000; 0xff00] (fun state ->
    assert_equal 0xfccd state.ac);
  "jneg true" >:: make_test [0x0004; 0xc003; 0xff00; 0xff00; 0x8888] (fun state ->
    assert_equal 0x0004 state.pc);
  "jneg false" >:: make_test [0x0004; 0xc003; 0xff00; 0xff00; 0x0000] (fun state ->
    assert_equal 0x0003 state.pc);
  "jnze true" >:: make_test [0x0004; 0xd003; 0xff00; 0xff00; 0x0001] (fun state ->
    assert_equal 0x0004 state.pc);
  "jnze false" >:: make_test [0x0004; 0xd003; 0xff00; 0xff00; 0x0000] (fun state ->
    assert_equal 0x0003 state.pc);
  "call" >:: make_test [0xe002; 0xff00; 0xff00] (fun state ->
    assert_equal 0x0003 state.pc;
    assert_equal 0x0f7f state.sp;
    assert_equal 0x0001 (State.read state state.sp));
  "pshi" >:: make_test [0x7003; 0xf000; 0xff00; 0x1234] (fun state ->
    assert_equal 0x1234 (State.read state state.sp);
    assert_equal 0x0f7f state.sp);
  "popi" >:: make_test [0x7670; 0xf400; 0x7005; 0xf200; 0xff00; 0x1234] (fun state ->
    assert_equal 0x0670 (State.read state 0x0005);
    assert_equal 0x0f80 state.sp);
  "push" >:: make_test [0x7670; 0xf400; 0xff00] (fun state ->
    assert_equal 0x0670 (State.read state state.sp);
    assert_equal 0x0f7f state.sp);
  "pop" >:: make_test [0x7670; 0xf400; 0xf600; 0xff00] (fun state ->
    assert_equal 0x0670 state.ac;
    assert_equal 0x0f80 state.sp);
  "retn" >:: make_test [0xe002; 0xff00; 0xf800; 0xff00] (fun state ->
    assert_equal 0x0002 state.pc;
    assert_equal 0x0f80 state.sp);
  "swap" >:: make_test [0x7890; 0xfa00; 0xff00] (fun state ->
    assert_equal 0x0f80 state.ac;
    assert_equal 0x0890 state.sp);
  "insp" >:: make_test [0xfc01; 0xff00] (fun state ->
    assert_equal 0x0f81 state.sp);
  "desp" >:: make_test [0xfe10; 0xff00] (fun state ->
    assert_equal 0x0f70 state.sp);
]
let _ = run_test_tt_main tests