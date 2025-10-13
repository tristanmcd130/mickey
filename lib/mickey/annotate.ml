(* let rec annotate type_defs type_env: unit Stmt.t -> Type.t Stmt.t = function
| Stmt.SProgram ss -> SProgram (List.map (annotate type_defs type_env) ss)
| SFun (n, ps, t, ls, b) ->
  if n = "main" && (ps <> [] || t <> TInt) then
    failwith "main function must have type () -> int";
  Env.add type_env n (Type.TArrow (List.map snd ps, t));
  let local_type_env = Env.create (Some type_env) ps in
  List.iter (fun (n, t, v) ->
    assert_type type_defs local_type_env v t;
    Env.add local_type_env n t) ls;
  assert_type type_defs local_type_env b t;
  SFun (n, ps, t, ls, annotate_exp type_defs type_env b)
| SVar (n, t, v) ->
  assert_type type_defs type_env v t;
  Env.add type_env n t
| SSig (n, t) -> Env.add type_env n t
| SImport _ -> failwith "import still present even after preprocessing"
| STypeDef (n, t) -> Hashtbl.replace type_defs n t
and annotate_exp type_defs type_env (exp, ()): Type.t Exp.t =
  match exp with
  | Exp.EInt i -> (Exp.EInt i, Type.TInt)
  | ECall (n, a) ->
    (match Env.find type_env n with
    | Type.TArrow (ps, r) ->
      if List.length a <> List.length ps then
        failwith (Printf.sprintf "Function %s expected %d arguments, but received %d" n (List.length ps) (List.length a));
      List.iter (fun (v, t) -> assert_type type_defs type_env v t) (List.combine a ps);
      (ECall (n, a), r)
    | _ -> failwith (n ^ " is not a function, cannot be called"))
  | EVar n -> Env.find type_env n
  | EUnary (UNeg, r) ->
    assert_type type_defs type_env r TInt;
    TInt
  | EUnary (UNot, r) ->
    assert_type type_defs type_env r TBool;
    TBool
  | EUnary (UDeref, r) ->
    (match annotate_exp type_defs type_env r with
    | TPtr t -> t
    | _ -> failwith "Not a pointer, cannot be dereferenced")
  | EBinary (l, BAdd, r) | EBinary (l, BSub, r) | EBinary (l, BMul, r) | EBinary (l, BDiv, r) ->
    assert_type type_defs type_env l TInt;
    assert_type type_defs type_env r TInt;
    TInt
  | EBinary (l, BEQ, r) | EBinary (l, BNE, r) ->
    assert_type type_defs type_env l (annotate_exp type_defs type_env r);
    TBool
  | EBinary (l, BGT, r) | EBinary (l, BLT, r) | EBinary (l, BGE, r) | EBinary (l, BLE, r) ->
    assert_type type_defs type_env l TInt;
    assert_type type_defs type_env r TInt;
    TBool
  | EBinary (l, BAnd, r) | EBinary (l, BOr, r) ->
    assert_type type_defs type_env l TBool;
    assert_type type_defs type_env r TBool;
    TBool
  | EPtrSet (e, v) ->
    let e_type = match annotate_exp type_defs type_env e with
    | TPtr t -> t
    | _ -> failwith "Not a pointer, cannot be assigned through" in
    assert_type type_defs type_env v e_type;
    TUnit
  | ESet (n, v) ->
    assert_type type_defs type_env v (Env.find type_env n);
    TUnit
  | EBreak e -> annotate_exp type_defs type_env e
  | EBool _ -> TBool
  | EIf (c, t, e) ->
    assert_type type_defs type_env c TBool;
    let e_type = annotate_exp type_defs type_env e in
    assert_type type_defs type_env t e_type;
    e_type
  | EWhile (c, b) ->
    assert_type type_defs type_env c TBool;
    assert_type type_defs type_env b TUnit;
    TUnit
  | EAs (e, t) ->
    annotate_exp type_defs type_env e |> ignore;
    t
  | EAddrOf n -> TPtr (annotate_exp type_defs type_env (EVar n, ()))
  | EUnit -> TUnit
  | EString _ -> TPtr TChar
  | EBlock [] -> TUnit
  | EBlock [e] -> annotate_exp type_defs type_env e
  | EBlock (e :: es) ->
    annotate_exp type_defs type_env e |> ignore;
    annotate_exp type_defs type_env (EBlock es, ())
  | EIndex (e, i) ->
    (match annotate_exp type_defs type_env e with
    | TPtr t ->
      assert_type type_defs type_env i TInt;
      t
    | _ -> failwith "Not a pointer, cannot be dereferenced")
  | EIndexSet (e, i, v) ->
    (match annotate_exp type_defs type_env e with
    | TPtr t ->
      assert_type type_defs type_env i TInt;
      assert_type type_defs type_env v t;
      TUnit
    | _ -> failwith "Not a pointer, cannot be dereferenced")
  | EChar _ -> TChar
  | EStruct (n, vs) ->
    (match Hashtbl.find_opt type_defs n with
    | None -> failwith ("Type " ^ n ^ " undefined")
    | Some (TStruct fs) ->
      List.iter (fun (k, v) -> assert_type type_defs type_env v (List.assoc k fs)) vs;
      TName n
    | _ -> failwith (n ^ " is not a struct type"))
and assert_type type_defs type_env exp type' =
  let actual_type = annotate_exp type_defs type_env exp in
  if not (Type.equal type_defs type' actual_type) then
    failwith ("Expected a value of type " ^ Type.to_string type' ^ ", but received one of type " ^ Type.to_string actual_type) *)
let rec annotate type_defs type_env = function
| Stmt.SProgram ss -> Stmt.SProgram (List.map (annotate type_defs type_env) ss)
| SFun (n, ps, t, ls, b) ->
  if n = "main" && (ps <> [] || t <> TInt) then
    failwith "main function must have type () -> int";
  Env.add type_env n (Type.TArrow (List.map snd ps, t));
  let local_type_env = Env.create (Some type_env) ps in
  let ls' = List.map (fun (l, t, v) ->
    let (va, vt) = annotate_exp type_defs local_type_env v in
    assert_equal_type type_defs vt t;
    Env.add local_type_env l t;
    (l, t, (va, vt))) ls in
  let (ba, bt) = annotate_exp type_defs local_type_env b in
  assert_equal_type type_defs bt t;
  SFun (n, ps, t, ls', (ba, bt))
| SVar (n, t, v) ->
  Env.add type_env n t;
  let (va, vt) = annotate_exp type_defs type_env v in
  assert_equal_type type_defs vt t;
  SVar (n, t, (va, vt))
| SImport _ -> failwith "import still present even after preprocessing"
| SSig (n, t) ->
  Env.add type_env n t;
  SSig (n, t)
| STypeDef (n, t) ->
  Hashtbl.replace type_defs n t;
  STypeDef (n, t)
and annotate_exp type_defs type_env (exp, ()) =
  match exp with
  | Exp.EInt i -> (Exp.EInt i, Type.TInt)
  | ECall (n, a) ->
    let (ps, r) = match Env.find type_env n with
    | TArrow (ps, r) -> (ps, r)
    | _ -> failwith (n ^ " is not a function, cannot be called") in
    if List.length a <> List.length ps then
      failwith (Printf.sprintf "Function %s expected %d arguments, but received %d" n (List.length ps) (List.length a));
    let a' = List.map (fun (t, v) ->
      let (va, vt) = annotate_exp type_defs type_env v in
      assert_equal_type type_defs vt t;
      (va, vt)) (List.combine ps a) in
    (ECall (n, a'), r)
  | EVar n -> (EVar n, Env.find type_env n)
  | EUnary (UNeg, r) ->
    let (ra, rt) = annotate_exp type_defs type_env r in
    assert_equal_type type_defs rt TInt;
    (EUnary (UNeg, (ra, rt)), TInt)
  | EUnary (UNot, r) ->
    let (ra, rt) = annotate_exp type_defs type_env r in
    assert_equal_type type_defs rt TBool;
    (EUnary (UNot, (ra, rt)), TBool)
  | EUnary (UDeref, r) ->
    let (ra, rt) = annotate_exp type_defs type_env r in
    (match rt with
    | TPtr t -> (EUnary (UDeref, (ra, rt)), t)
    | _ -> failwith "Not a pointer, cannot be dereferenced")
  | EBinary (l, (BAdd as o), r) | EBinary (l, (BSub as o), r) | EBinary (l, (BMul as o), r) | EBinary (l, (BDiv as o), r) | EBinary (l, (BMod as o), r) ->
    let (la, lt) = annotate_exp type_defs type_env l in
    let (ra, rt) = annotate_exp type_defs type_env r in
    assert_equal_type type_defs lt TInt;
    assert_equal_type type_defs rt TInt;
    (EBinary ((la, lt), o, (ra, rt)), TInt)
  | EBinary (l, (BEQ as o), r) | EBinary (l, (BNE as o), r) ->
    let (la, lt) = annotate_exp type_defs type_env l in
    let (ra, rt) = annotate_exp type_defs type_env r in
    assert_equal_type type_defs lt rt;
    (EBinary ((la, lt), o, (ra, rt)), TBool)
  | EBinary (l, (BGT as o), r) | EBinary (l, (BLT as o), r) | EBinary (l, (BGE as o), r) | EBinary (l, (BLE as o), r) ->
    let (la, lt) = annotate_exp type_defs type_env l in
    let (ra, rt) = annotate_exp type_defs type_env r in
    assert_equal_type type_defs lt TInt;
    assert_equal_type type_defs rt TInt;
    (EBinary ((la, lt), o, (ra, rt)), TBool)
  | EBinary (l, (BAnd as o), r) | EBinary (l, (BOr as o), r) ->
    let (la, lt) = annotate_exp type_defs type_env l in
    let (ra, rt) = annotate_exp type_defs type_env r in
    assert_equal_type type_defs lt TBool;
    assert_equal_type type_defs rt TBool;
    (EBinary ((la, lt), o, (ra, rt)), TBool)
  | EPtrSet (p, v) ->
    let (pa, pt) = annotate_exp type_defs type_env p in
    let (va, vt) = annotate_exp type_defs type_env v in
    (match pt with
    | TPtr t ->
      assert_equal_type type_defs vt t;
      (EPtrSet ((pa, pt), (va, vt)), TUnit)
    | _ -> failwith "Not a pointer, cannot be assigned through")
  | ESet (n, v) ->
    let (va, vt) = annotate_exp type_defs type_env v in
    assert_equal_type type_defs vt (Env.find type_env n);
    (ESet (n, (va, vt)), TUnit)
  | EBreak e ->
    let (ea, et) = annotate_exp type_defs type_env e in
    (EBreak (ea, et), et)
  | EBool b -> (EBool b, TBool)
  | EIf (c, t, e) ->
    let (ca, ct) = annotate_exp type_defs type_env c in
    let (ta, tt) = annotate_exp type_defs type_env t in
    let (ea, et) = annotate_exp type_defs type_env e in
    assert_equal_type type_defs ct TBool;
    assert_equal_type type_defs tt et;
    (EIf ((ca, ct), (ta, tt), (ea, et)), tt)
  | EWhile (c, b) ->
    let (ca, ct) = annotate_exp type_defs type_env c in
    let (ba, bt) = annotate_exp type_defs type_env b in
    assert_equal_type type_defs ct TBool;
    assert_equal_type type_defs bt TUnit;
    (EWhile ((ca, ct), (ba, bt)), TUnit)
  | EAs (e, t) ->
    let (ea, _) = annotate_exp type_defs type_env e in
    (ea, t)
  | EAddrOf n ->
    let (_, nt) = annotate_exp type_defs type_env (EVar n, ()) in
    (EAddrOf n, TPtr nt)
  | EUnit -> (EUnit, TUnit)
  | EString s -> (EString s, TPtr TChar)
  | EBlock [] -> (EBlock [], TUnit)
  | EBlock es ->
    let es' = List.map (annotate_exp type_defs type_env) es in
    (EBlock es', es' |> List.rev |> List.hd |> snd)
  | EIndex (e, i) ->
    let (ea, et) = annotate_exp type_defs type_env e in
    let (ia, it) = annotate_exp type_defs type_env i in
    assert_equal_type type_defs it TInt;
    (match et with
    | TPtr t -> (EIndex ((ea, et), (ia, it)), t)
    | _ -> failwith "Not a pointer, cannot be dereferenced")
  | EIndexSet (e, i, v) ->
    let (ea, et) = annotate_exp type_defs type_env e in
    let (ia, it) = annotate_exp type_defs type_env i in
    let (va, vt) = annotate_exp type_defs type_env v in
    assert_equal_type type_defs it TInt;
    (match et with
    | TPtr t ->
      assert_equal_type type_defs vt t;
      (EIndexSet ((ea, et), (ia, it), (va, vt)), TUnit)
    | _ -> failwith "Not a pointer, cannot be dereferenced")
  | EChar c -> (EChar c, TChar)
  | EStruct (n, vs) ->
    let fs = match Hashtbl.find type_defs n with
    | TStruct fs -> fs
    | t -> failwith (n ^ " is not a struct type, instead it is " ^ Type.to_string t) in
    let lacking = List.filter_map (fun (f, _) -> if List.mem_assoc f vs then None else (Some f)) fs in
    let extra = List.filter_map (fun (f, _) -> if List.mem_assoc f fs then None else (Some f)) vs in
    if List.length lacking > 0 then
      failwith (n ^ " was not given all necessary fields: " ^ (String.concat ", " lacking));
    if List.length extra > 0 then
      failwith (n ^ " was given undefined fields: " ^ (String.concat ", " extra));
    let vs' = List.map (fun (f, v) ->
      let (va, vt) = annotate_exp type_defs type_env v in
      assert_equal_type type_defs vt (List.assoc f fs);
      (f, (va, vt))) vs in
    (EStruct (n, vs'), TName n)
  | EDot (e, f) ->
    let (ea, et) = annotate_exp type_defs type_env e in
    (match et with
    | TName n ->
      (match Hashtbl.find type_defs n with
      | TStruct fs ->
        (match List.assoc_opt f fs with
        | None -> failwith (Type.to_string (TStruct fs) ^ " has no field " ^ f)
        | Some t -> (EDot ((ea, et), f), t))
      | t -> failwith (n ^ " is not a struct type, instead it is " ^ Type.to_string t))
    | t -> failwith (Type.to_string t ^ " is not a struct type (is it even possible to get here?)"))
and assert_equal_type type_defs actual expected =
  if not (Type.equal type_defs actual expected) then
    failwith ("Expected a value of type " ^ Type.to_string expected ^ ", but received one of type " ^ Type.to_string actual)