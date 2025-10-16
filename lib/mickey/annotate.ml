let rec annotate type_defs type_env = function
| Stmt.SProgram ss -> Stmt.SProgram (List.map (annotate type_defs type_env) ss)
| SFun (n, ps, t, ls, b) ->
  if n = "main" && (ps <> [] || t <> TInt) then
    failwith "main function must have type () -> int";
  Env.add type_env n (Type.TArrow (List.map snd ps, t));
  let local_type_env = Env.create (Some type_env) ps in
  let ls' = List.map (fun (l, t, v) ->
    let (va, vt) = annotate_exp type_defs local_type_env v in
    if match va with Exp.EString _ | EArray _ -> true | _ -> false then
      failwith ("Assigning array/string literal to a local (" ^ l ^ ") is not allowed. Only assign them to globals.");
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
  | ESet (n, v) ->
    let (na, nt) = annotate_exp type_defs type_env n in
    let (va, vt) = annotate_exp type_defs type_env v in
    assert_equal_type type_defs vt nt;
    if match va with EString _ | EArray _ -> true | _ -> false then
      failwith "Assigning array/string literal to a local is not allowed. Only assign them to globals.";
    (ESet ((na, nt), (va, vt)), TUnit)
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
    | _ -> failwith "Not a pointer, cannot be indexed")
  (* | EIndexSet (e, i, v) ->
    let (ea, et) = annotate_exp type_defs type_env e in
    let (ia, it) = annotate_exp type_defs type_env i in
    let (va, vt) = annotate_exp type_defs type_env v in
    assert_equal_type type_defs it TInt;
    (match et with
    | TPtr t ->
      assert_equal_type type_defs vt t;
      (EIndexSet ((ea, et), (ia, it), (va, vt)), TUnit)
    | _ -> failwith "Not a pointer, cannot be indexed") *)
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
  | EArray es ->
    let es' = List.map (annotate_exp type_defs type_env) es in
    let t =
      if List.length es' > 0 then
        es' |> List.hd |> snd
      else
        TUnit in
    List.iter (fun (_, t') -> assert_equal_type type_defs t' t) es';
    (EArray es', TPtr t)
  | EAddrOf n ->
    let (na, nt) = annotate_exp type_defs type_env n in
    (EAddrOf (na, nt), TPtr nt)
and assert_equal_type type_defs actual expected =
  if not (Type.equal type_defs actual expected) then
    failwith ("Expected a value of type " ^ Type.to_string expected ^ ", but received one of type " ^ Type.to_string actual)