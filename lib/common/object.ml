(*
uint16 length of symbol table
uint16 length of relocation table

symbol table:
uint32 hash (hash of the label string)
uint16 offset (where in the code is it defined, what is its actual value?)
hash
offset
hash
offset
...

relocation table:
uint16 offset (in the code where the label is referenced)
uint32 hash
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
  label_uses: (int, string) Hashtbl.t;
  code: Buffer.t;
}

let create () = {labels = Hashtbl.create 10; label_uses = Hashtbl.create 10; code = Buffer.create 100}
let add_word object' word = Buffer.add_uint16_be object'.code word
let add_label object' label = Hashtbl.replace object'.labels (String.hash label) (Buffer.length object'.code)
let use_label object' label = Hashtbl.replace object'.label_uses (Buffer.length object'.code) (String.hash label)
let to_bytes object' =
  let buffer = Buffer.create 100 in
  Buffer.add_uint16_be buffer (Hashtbl.length object'.labels);
  Buffer.add_uint16_be buffer (Hashtbl.length object'.label_uses);
  Hashtbl.iter (fun k v ->
    Buffer.add_int32_be buffer (Int32.of_int k);
    Buffer.add_uint16_be buffer v) object'.labels;
  Hashtbl.iter (fun k v ->
    Buffer.add_uint16_be buffer k;
    Buffer.add_int32_be buffer (Int32.of_int v)) object'.label_uses;
  Buffer.add_buffer buffer object'.code;
  Buffer.to_bytes buffer