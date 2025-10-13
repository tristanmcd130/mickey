type t =
| TInt
| TUnit
| TArrow of t list * t
| TBool
| TPtr of t
| TChar
| TStruct of (string * t) list
| TOpaque (* or incomplete for recursive types *)
| TName of string

val to_string: t -> string
val equal: (string, t) Hashtbl.t -> t -> t -> bool