type t =
| TInt
| TUnit
| TArrow of t list * t
| TBool
| TPtr of t
| TString

val to_string: t -> string