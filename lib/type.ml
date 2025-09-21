type t =
| TInt
| TUnit
| TArrow of t list * t
| TBool
| TPtr of t

let rec to_string = function
| TInt -> "int"
| TUnit -> "unit"
| TArrow (ps, r) -> Printf.sprintf "(%s) -> %s" (List.map to_string ps |> String.concat ", ") (to_string r)
| TBool -> "bool"
| TPtr t -> to_string t ^ " ptr"