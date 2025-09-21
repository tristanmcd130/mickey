type t

val create: unit -> t
val add_instructions: t -> Instruction.t list -> unit
val add_constant: t -> string -> int -> unit
val new_label: t -> string
val to_string: t -> string