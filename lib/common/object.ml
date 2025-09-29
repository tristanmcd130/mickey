(*
uint16 number of strings
uint16 length of symbol table
uint16 length of relocation table

strings:
uint8 length
uint8 char
...
length
char
...
length
char
...

symbol table:
uint16 index (into the strings)
uint16 offset (where in the code is it defined, what is its actual value?)
index
offset
index
offset
...

relocation table:
uint16 offset (in the code where the label is referenced)
uint16 index
...

code:
desp 1
loco 0
stol 0
lodl 2
jzer mulEnd (all labels are replaced with 0; if an address genuinely is a number it just wont have a table entry)
subd c1
...
*)
type t = {
  labels: (string, int) Hashtbl.t;
  relocations: (int, string) Hashtbl.t;
  code: Bytes.t; (* TODO: try changing this to Bytes.t *)
}

module StringSet = Set.Make(struct
  type t = string
  let compare = String.compare
end)

let to_bytes object' =
  let buffer = Buffer.create 100 in
  (* Printf.printf "BUFFER SIZE: %d\n" (Buffer.length buffer); *)
  let all_labels = Hashtbl.to_seq_keys object'.labels |> StringSet.of_seq |> StringSet.add_seq (Hashtbl.to_seq_values object'.relocations) in
  Buffer.add_uint16_be buffer (StringSet.cardinal all_labels);
  Buffer.add_uint16_be buffer (Hashtbl.length object'.labels);
  Buffer.add_uint16_be buffer (Hashtbl.length object'.relocations);
  let label_order = StringSet.to_seq all_labels |> Seq.mapi (fun i l -> (l, i)) |> Hashtbl.of_seq in
  StringSet.iter (fun k ->
    Buffer.add_uint8 buffer (String.length k);
    Buffer.add_string buffer k) all_labels;
  Hashtbl.iter (fun k v ->
    Buffer.add_uint16_be buffer (Hashtbl.find label_order k);
    Buffer.add_uint16_be buffer v) object'.labels;
  Hashtbl.iter (fun k v ->
    Buffer.add_uint16_be buffer k;
    Buffer.add_uint16_be buffer (Hashtbl.find label_order v)) object'.relocations;
  Buffer.add_bytes buffer object'.code;
  Buffer.to_bytes buffer
let of_bytes bytes =
  let num_strings = Bytes.get_uint16_be bytes 0 in
  let symbol_table_size = Bytes.get_uint16_be bytes 2 in
  let relocation_table_size = Bytes.get_uint16_be bytes 4 in
  let labels = Dynarray.create () in
  let i = ref 6 in
  for _ = 1 to num_strings do
    let length = Bytes.get_uint8 bytes !i in
    let label = Bytes.sub_string bytes (!i + 1) length in
    Dynarray.add_last labels label;
    (* Printf.printf "label: %x %s\n" !i label; *)
    i := !i + length + 1
  done;
  let symbol_table = Hashtbl.create symbol_table_size in
  for _ = 1 to symbol_table_size do
    (* Printf.printf "symbol: %x %04x\n" !i (Bytes.get_uint16_be bytes !i); *)
    Hashtbl.replace symbol_table (Dynarray.get labels (Bytes.get_uint16_be bytes !i)) (Bytes.get_uint16_be bytes (!i + 2));
    i := !i + 4
  done;
  let relocation_table = Hashtbl.create relocation_table_size in
  for _ = 1 to relocation_table_size do
    Hashtbl.replace relocation_table (Bytes.get_uint16_be bytes !i) (Dynarray.get labels (Bytes.get_uint16_be bytes (!i + 2)));
    i := !i + 4
  done;
  {labels = symbol_table; relocations = relocation_table; code = Bytes.sub bytes !i (Bytes.length bytes - !i)}