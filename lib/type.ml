type t =
| TInt
| TUnit
| TArrow of t list * t
| TBool

let rec to_string = function
| TInt -> "int"
| TUnit -> "unit"
| TArrow (ps, r) -> Printf.sprintf "(%s) -> %s" (List.map to_string ps |> String.concat ", ") (to_string r)
| TBool -> "bool"