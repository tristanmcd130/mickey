open Simulator

let () =
  if Array.length Sys.argv <> 2 then
    (prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <program>");
    exit 1);
  let rcvr_on = ref false in
  let xmtr_on = ref false in
  let buffer = Queue.create () in
  let state = State.create (In_channel.with_open_bin Sys.argv.(1) In_channel.input_all |> Bytes.of_string)
    ~read_callbacks: [
      (4092, fun _ ->
        if Queue.is_empty buffer then
          0
        else
          ((* Printf.printf "CHAR RECEIVED: %d\n" (Queue.peek buffer |> Char.code); *)
          Queue.take buffer |> Char.code));
      (4093, fun _ ->
        if !rcvr_on then
          (if Queue.is_empty buffer then
            (read_line () ^ "\n") |> String.iter (fun x -> Queue.add x buffer);
          10)
        else
          0);
      (4095, fun _ ->
        if !xmtr_on then
          10
        else
          0);
    ]
    ~write_callbacks: [
      (4093, fun _ value -> rcvr_on := value land 8 = 8);
      (4094, fun _ value ->
        if !xmtr_on then
          ((* Printf.printf "CHAR PRINTED: %d\n" value; *)
          value |> Char.chr |> print_char));
      (4095, fun _ value -> xmtr_on := value land 8 = 8);
    ] in
  State.run state;
  while State.debug state do
    State.run state;
  done;