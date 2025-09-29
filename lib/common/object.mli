type t = {
  labels: (string, int) Hashtbl.t;
  relocations: (int, string) Hashtbl.t;
  code: Bytes.t; (* TODO: try changing this to Bytes.t *)
}

val to_bytes: t -> bytes
val of_bytes: bytes -> t