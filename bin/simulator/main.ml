open Simulator

let () =
  if Array.length Sys.argv <> 2 then
    (prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <program>");
    exit 1);
  let state = State.create (In_channel.with_open_bin Sys.argv.(1) In_channel.input_all |> Bytes.of_string) in
  while true do
    State.run state;
    State.debug state
  done