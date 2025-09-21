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
| SVar (n, t, v) ->
  assert_type type_env v t;
  Env.add type_env n t
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
| EUnary (UNot, r) ->
  assert_type type_env r TBool;
  TBool
| EUnary (UDeref, r) ->
  (match type_of type_env r with
  | TPtr t -> t
  | _ -> failwith "Not a pointer, cannot be dereferenced")
| EBinary (l, BAdd, r) | EBinary (l, BSub, r) | EBinary (l, BMul, r) | EBinary (l, BDiv, r) ->
  assert_type type_env l TInt;
  assert_type type_env r TInt;
  TInt
| EBinary (l, BEQ, r) | EBinary (l, BNE, r) ->
  assert_type type_env l (type_of type_env r);
  TBool
| EBinary (l, BGT, r) | EBinary (l, BLT, r) | EBinary (l, BGE, r) | EBinary (l, BLE, r) ->
  assert_type type_env l TInt;
  assert_type type_env r TInt;
  TBool
| EBinary (l, BAnd, r) | EBinary (l, BOr, r) ->
  assert_type type_env l TBool;
  assert_type type_env r TBool;
  TBool
| EBinary (l, BPtrSet, r) ->
  let l_type = match type_of type_env l with
  | TPtr t -> t
  | _ -> failwith "Not a pointer, cannot be assigned through" in
  assert_type type_env r l_type;
  TUnit
| ESet (n, v) ->
  assert_type type_env v (Env.find type_env n);
  TUnit
| EBreak e -> type_of type_env e
| EBool _ -> TBool
| EIf (c, t, e) ->
  assert_type type_env c TBool;
  let e_type = type_of type_env e in
  assert_type type_env t e_type;
  e_type
| EWhile (c, b) ->
  assert_type type_env c TBool;
  assert_type type_env b TUnit;
  TUnit
| EAs (e, t) -> t
| EAddrOf n -> TPtr (type_of type_env (EVar n))
and assert_type type_env exp type' =
  let actual_type = type_of type_env exp in
  if actual_type <> type' then
    failwith ("Expected a value of type " ^ Type.to_string type' ^ ", but received one of type " ^ Type.to_string actual_type)