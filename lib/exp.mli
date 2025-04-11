type t =
| EBool of bool
| EInt of int
| EString of string
| EVar of string
| ECall of string * t list
| EAs of t * Type.t
| EAt of t