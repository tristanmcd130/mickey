open OUnit2
open Common
open Linker

let make_test exe objs _ =
  let buffer = Buffer.create 10 in
  List.iter (Buffer.add_uint16_be buffer) exe;
  assert_equal (Buffer.to_bytes buffer) (Link.link objs) ~printer: (fun x -> Bytes.to_string x |> String.escaped)
let make_error_test msg objs _ =
  assert_raises (Failure msg) (fun _ -> Link.link objs)
let tests = "linker tests" >::: [
  "empty" >:: make_test [] [Object.create [] [] []];
  "basic label" >:: make_test [0x300a] [Object.create [("label", 10)] [(0, "label")] [0x3000]];
  "undefined label" >:: make_error_test "Object not fully resolved: r at 0000" [Object.create [] [(0, "r")] [0x3000]];
  "multiple uses of label" >:: make_test [0x300a; 0x400a] [Object.create [("label", 10)] [(0, "label"); (1, "label")] [0x3000; 0x4000]];
  "absolute address" >:: make_test [0x3067; 0x400a] [Object.create [("label", 10)] [(1, "label")] [0x3067; 0x4000]];
  "multiple objects" >:: make_test [0x3001; 0x0000; 0x0003; 0x0000] [
    Object.create [("a", 1)] [(0, "a")] [0x3000; 0x0000];
    Object.create [("b", 1)] [(0, "b")] [0x0000; 0x0000];
  ];
  "labels from other objects" >:: make_test [0x3003; 0x0000; 0x0001; 0x0000] [
    Object.create [("a", 1)] [(0, "b")] [0x3000; 0x0000];
    Object.create [("b", 1)] [(0, "a")] [0x0000; 0x0000];
  ];
  "local label" >:: make_test [0x3001; 0x0000; 0x0003; 0x0000] [
    Object.create [("a", 1)] [(0, "a")] [0x3000; 0x0000];
    Object.create [("a", 1)] [(0, "a")] [0x0000; 0x0000];
  ];
]
let _ = run_test_tt_main tests