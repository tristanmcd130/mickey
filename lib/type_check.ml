let rec process_defs = function
| Stmt.SBlock [] -> []
| SBlock (s :: ss) -> process_defs s @ process_defs (SBlock ss)
| SGlobals (gs, _) -> gs
| SFun (n, ps, t, _, _) -> [(n, TFun (List.map snd ps, t))]
| _ -> []

let rec type_check stmt type_env =
  match stmt with
  | Stmt.SBlock [] -> Type.TVoid
  | SBlock [s] -> type_check s type_env
  | SBlock (s :: ss) ->
    type_check s type_env |> ignore;
    type_check (SBlock ss) type_env
  | SGlobals (gs, b) -> type_check b type_env
  | SFun (n, ps, t, ls, b) ->
    let t' = type_check b (ps @ ls @ type_env) in
    (if t = t' then
      TVoid
    else
      failwith ("Function " ^ n ^ " is declared to return " ^ Type.to_string t ^ " but actually returns " ^ Type.to_string t'))
  | SAssign (n, v) ->
    (match List.assoc_opt n type_env with
    | Some t ->
      (if t = TVoid then
        failwith ("Cannot assign anything to the variable " ^ n ^ " of type void")
      else if t = type_of v type_env then
        TVoid
      else
        failwith ("Cannot assign " ^ (type_of v type_env |> Type.to_string) ^ " to the variable " ^ n ^ " of type " ^ Type.to_string t))
    | None -> failwith ("Variable " ^ n ^ " not defined"))
  | SCall (n, a) ->
    (match List.assoc_opt n type_env with
    | Some (TFun (ps, r) as t) ->
      (if ps = List.map (fun x -> type_of x type_env) a then
        TVoid
      else
        failwith ("Function " ^ n ^ " has type " ^ Type.to_string t ^ ", but its arguments have types " ^ (List.map (fun x -> type_of x type_env) a |> List.map Type.to_string |> String.concat ", ")))
    | Some _ -> failwith (n ^ " is not a function, it cannot be called")
    | None -> failwith ("Function " ^ n ^ " not defined"))
  | SIf (c, t, e) ->
    (if type_of c type_env = TBool then
      let t' = type_check t type_env in
      if t' = type_check e type_env then
        t'
      else
        failwith ("Both if branches must have the same type: then branch is " ^ Type.to_string t' ^ ", but else branch is " ^ (type_check e type_env |> Type.to_string))
    else
      failwith ("If condition must be a bool, not a " ^ (type_of c type_env |> Type.to_string)))
  | SWhile (c, b) ->
    (if type_of c type_env = TBool then
      type_check b type_env
    else
      failwith ("While condition must be a bool, not a " ^ (type_of c type_env |> Type.to_string)))
  | SReturn e -> type_of e type_env
and type_of exp type_env =
  match exp with
  | EBool _ -> Type.TBool
  | EInt _ -> TInt
  | EVar n ->
    (match List.assoc_opt n type_env with
    | Some t -> t
    | None -> failwith ("Variable " ^ n ^ " not defined"))
  | ECall (n, a) ->
    (match List.assoc_opt n type_env with
    | Some (TFun (ps, r) as t) ->
      (if ps = List.map (fun x -> type_of x type_env) a then
        r
      else
        failwith ("Function " ^ n ^ " has type " ^ Type.to_string t ^ ", but its arguments have types " ^ (List.map (fun x -> type_of x type_env) a |> List.map Type.to_string |> String.concat ", ")))
    | Some _ -> failwith (n ^ " is not a function, it cannot be called")
    | None -> failwith ("Function " ^ n ^ " not defined"))