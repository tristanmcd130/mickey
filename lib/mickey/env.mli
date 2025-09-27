type 'a t

val create: 'a t option -> (string * 'a) list -> 'a t
val add: 'a t -> string -> 'a -> unit
val find: 'a t -> string -> 'a
val find_opt: 'a t -> string -> 'a option