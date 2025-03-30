type t =
| TVoid
| TBool
| TInt
| TPtr of t
| TFun of t list * t
| TNoReturn

val to_string: t -> string