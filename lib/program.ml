type t = {
  instructions: Instruction.t Dynarray.t;
  constants: (string, int) Hashtbl.t;
  mutable label_num: int;
}

let create () = {instructions = Dynarray.create (); constants = Hashtbl.create 10; label_num = 0}
let rec add_instruction program = function
| Instruction.ILoco (Int i) when i < 0 ->
  let label = Printf.sprintf "cn%d" ~+i in
  add_constant program label i;
  add_instruction program (ILodd (Label label))
| Instruction.ILoco (Int i) when i > 4095 ->
  let label = Printf.sprintf "c%d" i in
  add_constant program label i;
  add_instruction program (ILodd (Label label))
| i -> Dynarray.add_last program.instructions i
and add_constant program name value = Hashtbl.replace program.constants name value
let add_instructions program instructions = List.iter (add_instruction program) instructions
let new_label program =
  let label = Printf.sprintf "l%d" program.label_num in
  program.label_num <- program.label_num + 1;
  label
let to_string program =
  (Hashtbl.to_seq program.constants |> Seq.map (fun (k, v) -> Printf.sprintf "%s: %d\n" k v) |> List.of_seq |> String.concat "")
  ^ (Dynarray.map Instruction.to_string program.instructions |> Dynarray.to_list |> String.concat "\n")