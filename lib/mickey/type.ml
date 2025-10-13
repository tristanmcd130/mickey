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

let rec to_string = function
| TInt -> "int"
| TUnit -> "unit"
| TArrow (ps, r) -> Printf.sprintf "(%s) -> %s" (List.map to_string ps |> String.concat ", ") (to_string r)
| TBool -> "bool"
| TPtr t -> to_string t ^ " ptr"
| TChar -> "char"
| TStruct fs -> "{" ^ (fs |> List.map (fun (n, t) -> n ^ ": " ^ to_string t) |> String.concat ", ") ^ "}"
| TOpaque -> "<opaque>"
| TName n -> n
let rec equal type_defs type1 type2 =
  match (type1, type2) with
  | (TOpaque, _) | (_, TOpaque) -> false
  | (TName n, TName n') -> n = n'
  | (TName n, t) | (t, TName n) ->
    (match t with
    | TStruct _ -> false
    | t -> equal type_defs (Hashtbl.find type_defs n) t)
  | (TArrow (ps, r), TArrow (ps', r')) -> List.for_all2 (equal type_defs) ps ps' && equal type_defs r r'
  | (TPtr t, TPtr t') -> equal type_defs t t'
  | (TStruct fs, TStruct fs') -> List.for_all2 (fun (f, t) (f', t') -> f = f' && equal type_defs t t') fs fs'
  | (t1, t2) -> t1 = t2