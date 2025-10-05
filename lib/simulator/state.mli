type t = {
  mutable ac: int;
  mutable pc: int;
  mutable sp: int;
  memory: Bytes.t;
  read_callbacks: (int, t -> int) Hashtbl.t;
  write_callbacks: (int, t -> int -> unit) Hashtbl.t;
}

val create: ?read_callbacks: (int * (t -> int)) list -> ?write_callbacks: (int * (t -> int -> unit)) list -> bytes -> t
val read: t -> int -> int
val write: t -> int -> int -> unit
val run: t -> unit
val debug: t -> bool