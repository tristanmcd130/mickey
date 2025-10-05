type t =
| TInt
| TUnit
| TArrow of t list * t
| TBool
| TPtr of t
| TChar

val to_string: t -> string