open Simulator

let () =
  if Array.length Sys.argv <> 1 then
    (prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <program>");
    exit 1)
  else
    let state = State.create (In_channel.with_open_bin Sys.argv.(1) In_channel.input_all |> Bytes.of_string) in
    State.run state