type t = {
  instructions: Common.Instruction.t Dynarray.t;
  constants: (string, constant) Hashtbl.t;
  mutable label_num: int;
}
and constant =
| CInt of int
| CString of string

val create: unit -> t
val add_instructions: t -> Common.Instruction.t list -> unit
val add_constant: t -> string -> constant -> unit
val new_label: t -> string
val to_string: t -> string