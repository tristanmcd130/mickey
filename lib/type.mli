type t =
| TInt
| TUnit
| TArrow of t list * t
| TBool

val to_string: t -> string