let rec process_defs = function
| Stmt.SBlock [] -> []
| SBlock (s :: ss) -> process_defs s @ process_defs (SBlock ss)
| SImport f -> In_channel.open_text f |> Lexing.from_channel |> Parser.prog Lexer.read |> process_defs
| SGlobals (gs, _) -> gs
| SFun (n, ps, t, _, _) -> [(n, TFun (List.map snd ps, t))]
| _ -> []

let rec type_check stmt type_env =
  match stmt with
  | Stmt.SBlock [] -> Type.TNoReturn
  | SBlock [s] -> type_check s type_env
  | SBlock (s :: ss) ->
    type_check s type_env |> ignore;
    type_check (SBlock ss) type_env
  | SImport f -> type_check (In_channel.open_text f |> Lexing.from_channel |> Parser.prog Lexer.read) type_env
  | SGlobals (gs, b) -> type_check b type_env
  | SFun (n, ps, t, ls, b) ->
    let t' = type_check b (ps @ ls @ type_env) in
    if n = "main" && (t <> TInt || ps <> []) then
      failwith "Main function must take no arguments and return int"
    else if t = t' then
      TNoReturn
    else
      failwith ("Function " ^ n ^ " is declared to return " ^ Type.to_string t ^ " but actually returns " ^ Type.to_string t' ^ " (note: void must be explicitly returned with \"return\")")
  | SAssign (n, v) ->
    (match List.assoc_opt n type_env with
    | Some t ->
      if t = TVoid then
        failwith ("Cannot assign anything to the variable " ^ n ^ " of type void")
      else if t = type_of v type_env then
        TNoReturn
      else
        failwith ("Cannot assign " ^ (type_of v type_env |> Type.to_string) ^ " to the variable " ^ n ^ " of type " ^ Type.to_string t)
    | None -> failwith ("Variable " ^ n ^ " not defined"))
  | SPtrAssign (p, v) ->
    let t = (match type_of p type_env with
    | TPtr p -> p
    | t' -> failwith ("Attempting to assign to " ^ Type.to_string t' ^ ", which is not a pointer type")) in
    if t = type_of v type_env then
      TVoid
    else
      failwith ("Cannot assign " ^ (type_of v type_env |> Type.to_string) ^ " to a " ^ Type.to_string t ^ " ptr")
  | SCall (n, a) -> type_of_call n a type_env
  | SIf (c, t, e) ->
    if type_of c type_env = TBool then
      let t' = type_check t type_env in
      if t' = type_check e type_env then
        t'
      else
        failwith ("Both if branches must have the same type: then branch is " ^ Type.to_string t' ^ ", but else branch is " ^ (type_check e type_env |> Type.to_string))
    else
      failwith ("If condition must be a bool, not a " ^ (type_of c type_env |> Type.to_string))
  | SWhile (c, b) ->
    if type_of c type_env = TBool then
      type_check b type_env
    else
      failwith ("While condition must be a bool, not a " ^ (type_of c type_env |> Type.to_string))
  | SReturn e ->
    (match e with
    | Some v -> type_of v type_env
    | None -> TVoid)
  | SAsm _ -> TVoid
and type_of exp type_env =
  match exp with
  | EBool _ -> Type.TBool
  | EInt _ -> TInt
  | EString _ -> TPtr TInt 
  | EVar n ->
    (match List.assoc_opt n type_env with
    | Some t -> t
    | None -> failwith ("Variable " ^ n ^ " not defined"))
  | ECall (n, a) -> type_of_call n a type_env
  | EAs (_, t) -> t
  | EAt e ->
    (match type_of e type_env with
    | TPtr t -> t
    | t -> failwith ("Cannot dereference " ^ Type.to_string t ^ ", which is not a pointer type"))
and type_of_call name args type_env =
  let ts = List.map (fun x -> type_of x type_env) args in
  match name with
  | "eq" | "ne" ->
    if List.nth ts 0 <> List.nth ts 1 then
      failwith ("Operator " ^ name ^ " expects 2 arguments of the same type, but its arguments have types " ^ (ts |> List.map Type.to_string |> String.concat ", "))
    else
      TBool
  | n ->
    match List.assoc_opt n type_env with
    | Some (TFun (ps, r) as t) ->
      if ps = ts then
        r
      else
        failwith ("Function " ^ n ^ " has type " ^ Type.to_string t ^ ", but its arguments have types " ^ (ts |> List.map Type.to_string |> String.concat ", "))
    | Some _ -> failwith (n ^ " is not a function, it cannot be called")
    | None -> failwith ("Function " ^ n ^ " not defined")