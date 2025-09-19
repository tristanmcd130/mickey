let rec type_check type_env = function
| Stmt.SProgram [] -> ()
| SProgram (s :: ss) ->
  type_check type_env s;
  type_check type_env (SProgram ss)
| SFun (n, ps, t, ls, b) ->
  (if n = "main" && (ps <> [] || t <> TInt) then
    failwith "main function must have type () -> int");
  Env.add type_env n (Type.TArrow (List.map snd ps, t));
  assert_type (Env.create (Some type_env) (ps @ ls)) b t
and type_of type_env = function
| Exp.EInt i -> Type.TInt
| EBlock [] -> TUnit
| EBlock [e] -> type_of type_env e
| EBlock (e :: es) -> 
  type_of type_env e |> ignore;
  type_of type_env (EBlock es)
| ECall (n, a) ->
  (match Env.find type_env n with
  | Type.TArrow (ps, r) ->
    List.iter (fun (v, t) -> assert_type type_env v t) (List.combine a ps);
    r
  | _ -> failwith "Not a function")
| EVar n -> Env.find type_env n
| EUnary (UNeg, r) ->
  assert_type type_env r TInt;
  TInt
| EBinary (l, BAdd, r) | EBinary (l, BSub, r) ->
  assert_type type_env l TInt;
  assert_type type_env r TInt;
  TInt
| ESet (n, v) ->
  assert_type type_env v (Env.find type_env n);
  TUnit
| EBreak e -> type_of type_env e
| EBool b -> TBool
and assert_type type_env exp type' =
  let actual_type = type_of type_env exp in
  if actual_type <> type' then
    failwith ("Expected a value of type " ^ Type.to_string type' ^ ", but received one of type " ^ Type.to_string actual_type)