type t = {
  labels: (string, int) Hashtbl.t;
  relocations: (int, string) Hashtbl.t;
  code: Bytes.t;
}

val create: (string * int) list -> (int * string) list -> int list -> t
val to_bytes: t -> bytes
val of_bytes: bytes -> t