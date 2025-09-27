type t

val create: unit -> t
val add_word: t -> int -> unit
val add_label: t -> string -> unit
val use_label: t -> string -> unit
val to_bytes: t -> bytes