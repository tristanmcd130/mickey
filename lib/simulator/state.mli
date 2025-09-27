type t = {
  mutable ac: int;
  mutable pc: int;
  mutable sp: int;
  memory: Bytes.t;
  read_callbacks: (int, t -> unit) Hashtbl.t;
  write_callbacks: (int, t -> unit) Hashtbl.t;
}

val create: ?read_callbacks: (int, t -> unit) Hashtbl.t -> ?write_callbacks: (int, t -> unit) Hashtbl.t -> bytes -> t
val read: t -> int -> int
val write: t -> int -> int -> unit
val run: t -> unit