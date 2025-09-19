type t =
| AProgram of t list
| AFun of string * (string * Type.t) list * Type.t * t
| AInt of int
| ABlock of t list