type t =
| TVoid
| TBool
| TInt
| TPtr of t
| TFun of t list * t

val to_string: t -> string