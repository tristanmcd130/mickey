open Common
open Linker

let () =
  if Array.length Sys.argv < 2 then
    (prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " [input files] <output>");
    exit 1);
  let filenames = Dynarray.create () in
  for i = 1 to Array.length Sys.argv - 2 do
    Dynarray.add_last filenames Sys.argv.(i)
  done;
  let objects = Dynarray.map (fun x -> In_channel.with_open_bin x In_channel.input_all |> Bytes.of_string |> Object.of_bytes) filenames in
  let executable = Link.link (Dynarray.to_list objects) in
  Out_channel.with_open_bin Sys.argv.(Array.length Sys.argv - 1) (fun x -> output_bytes x executable)