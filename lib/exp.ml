type t =
| EBool of bool
| EInt of int
| EVar of string
| ECall of string * t list