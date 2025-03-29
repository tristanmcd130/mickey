type t =
| TVoid
| TBool
| TInt
| TPtr of t
| TFun of t list * t

let rec to_string = function
| TVoid -> "void"
| TBool -> "bool"
| TInt -> "int"
| TPtr t -> to_string t ^ " ptr"
| TFun (ps, r) -> "(" ^ (List.map to_string ps |> String.concat ", ") ^ ") -> " ^ to_string r