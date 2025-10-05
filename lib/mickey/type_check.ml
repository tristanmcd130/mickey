let rec type_check type_env = function
| Stmt.SProgram [] -> ()
| SProgram (s :: ss) ->
  type_check type_env s;
  type_check type_env (SProgram ss)
| SFun (n, ps, t, ls, b) ->
  (if n = "main" && (ps <> [] || t <> TInt) then
    failwith "main function must have type () -> int");
  Env.add type_env n (Type.TArrow (List.map snd ps, t));
  let local_type_env = Env.create (Some type_env) ps in
  List.iter (fun (n, t, v) ->
    assert_type local_type_env v t;
    Env.add local_type_env n t) ls;
  assert_type local_type_env b t
| SVar (n, t, v) ->
  assert_type type_env v t;
  Env.add type_env n t
| SSig (n, t) -> Env.add type_env n t
| SImport _ -> failwith "import still present even after preprocessing"
and type_of type_env = function
| Exp.EInt _ -> Type.TInt
| ECall (n, a) ->
  (match Env.find type_env n with
  | Type.TArrow (ps, r) ->
    if List.length a <> List.length ps then
      failwith (Printf.sprintf "Function %s expected %d arguments, but received %d" n (List.length ps) (List.length a));
    List.iter (fun (v, t) -> assert_type type_env v t) (List.combine a ps);
    r
  | _ -> failwith (n ^ " is not a function, cannot be called"))
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
| EPtrSet (e, v) ->
  let e_type = match type_of type_env e with
  | TPtr t -> t
  | _ -> failwith "Not a pointer, cannot be assigned through" in
  assert_type type_env v e_type;
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
| EAs (e, t) ->
  type_of type_env e |> ignore;
  t
| EAddrOf n -> TPtr (type_of type_env (EVar n))
| EUnit -> TUnit
| EString _ -> TString
| EBlock [] -> TUnit
| EBlock [e] -> type_of type_env e
| EBlock (e :: es) ->
  type_of type_env e |> ignore;
  type_of type_env (EBlock es)
and assert_type type_env exp type' =
  let actual_type = type_of type_env exp in
  if actual_type <> type' then
    failwith ("Expected a value of type " ^ Type.to_string type' ^ ", but received one of type " ^ Type.to_string actual_type)