open Common

let patch (object': Object.t): Object.t = 
  let bytes = object'.code in
  let relocations_copy = Hashtbl.copy object'.relocations in
  Hashtbl.iter (fun k v -> match Hashtbl.find_opt object'.labels v with
  | None -> ()
  | Some i ->
    Bytes.set_uint16_be bytes (k * 2) (Bytes.get_uint16_be bytes (k * 2) + i);
    Hashtbl.remove relocations_copy k) object'.relocations;
  {
    labels = object'.labels;
    relocations = relocations_copy;
    code = bytes;
  }
let link objects =
  let link2 (obj1: Object.t) (obj2: Object.t): Object.t = {
    labels =
      (let table = obj1.labels in
      Hashtbl.iter (fun k v -> Hashtbl.replace table k (v + Bytes.length obj1.code / 2)) obj2.labels;
      table);
    relocations =
      (let table = obj1.relocations in
      Hashtbl.iter (fun k v -> Hashtbl.replace table (k + Bytes.length obj1.code / 2) v) obj2.relocations;
      table);
    code = Bytes.cat obj1.code obj2.code;
  } in
  let exe = List.fold_left (fun x y -> patch (link2 x y)) (Object.create [] [] []) objects in
  if Hashtbl.length exe.relocations > 0 then
    failwith (Printf.sprintf "Object not fully resolved: %s" (Hashtbl.to_seq exe.relocations |> Seq.map (fun (k, v) -> Printf.sprintf "%s at %04x" v k) |> List.of_seq |> String.concat ", "));
  exe.code