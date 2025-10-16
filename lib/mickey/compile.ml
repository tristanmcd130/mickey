let rec find_first ?(index = 0) value = function
| [] -> failwith "Can't find value"
| v :: _ when v = value -> index
| _ :: vs -> find_first ~index: (index + 1) value vs
let rec exp_to_constant = function
| Exp.EInt i -> Program.CInt i
| EChar c -> CChar c
| EUnit | EBool false -> CInt 0
| EBool true -> CInt 1
| EString s -> CString s
| EArray es -> CArray (List.map (fun (e, _) -> exp_to_constant e) es)
| _ -> failwith "Invalid array value (try setting it with an index later)"
let rec compile program type_defs env = function
| Stmt.SProgram [] | SSig _ | STypeDef _ -> ()
| SProgram (s :: ss) ->
  compile program type_defs env s;
  compile program type_defs env (SProgram ss)
| SFun (n, ps, _, ls, b) ->
  Program.add_instructions program [
    ILabel n;
    ILodd (Label "fp");
    IPush;
    IDesp (List.length ls);
    ISwap;
    IStod (Label "fp");
    ISwap;
  ];
  let local_env = Env.create (Some env) (List.mapi (fun i (n, _) -> (n, i)) ((List.map (fun (n, t, _) -> (n, t)) ls) @ [("", Type.TUnit); ("", TUnit)] @ ps)) in
  List.iter (fun (n, t, v) -> compile_exp program type_defs local_env (Exp.ESet ((EVar n, t), v), Type.TUnit)) ls;
  compile_exp program type_defs local_env b;
  Program.add_instructions program [
    IStod (Label "tmp");
    IInsp (List.length ls);
    IPop;
    IStod (Label "fp");
    ILodd (Label "tmp");
    IRetn;
  ]
| SVar (n, _, (v, _)) ->
  (match v with
  | (EString _ | EArray _) as e ->
    let label = Printf.sprintf "a%d" (String.hash n) in
    Program.add_constant program label (exp_to_constant e);
    Program.add_constant program n (CLabel label)
  | e -> Program.add_constant program n (exp_to_constant e))
| SImport _ -> failwith "import still present even after preprocessing... and it's also not caught by type checking?"
and compile_exp program type_defs env (exp, type') =
  match exp with
  | Exp.EInt i -> Program.add_instructions program [ILoco (Int i)]
  | ECall (n, a) ->
    List.rev a |> List.iter (fun x -> compile_exp program type_defs env x; Program.add_instructions program [IPush]);
    Program.add_instructions program [
      ICall (Label n);
      IInsp (List.length a);
    ]
  | EVar n ->
    Program.add_instructions program (match Env.find_opt env n with
    | None -> [ILodd (Label n)]
    | Some i -> [ILoco (Int i); IAddd (Label "fp"); IPshi; IPop])
  | EUnary (UNeg, r) ->
    compile_exp program type_defs env r;
    Program.add_instructions program [
      IPush;
      ILoco (Int 0);
      ISubl 0;
      IInsp 1;
    ]
  | EUnary (UNot, r) ->
    compile_exp program type_defs env r;
    Program.add_instructions program [
      IPush;
      ILoco (Int 1);
      ISubl 0;
      IInsp 1;
    ]
  | EBinary (l, BAdd, r) ->
    compile_exp program type_defs env r;
    Program.add_instructions program [IPush];
    compile_exp program type_defs env l;
    Program.add_instructions program [
      IAddl 0;
      IInsp 1;
    ]
  | EBinary (l, BSub, r) ->
    compile_exp program type_defs env r;
    Program.add_instructions program [IPush];
    compile_exp program type_defs env l;
    Program.add_instructions program [
      ISubl 0;
      IInsp 1;
    ]
  | EBinary (l, BMul, r) -> compile_exp program type_defs env (ECall ("_mul", [l; r]), TInt)
  | EBinary (l, BDiv, r) -> compile_exp program type_defs env (ECall ("_div", [l; r]), TInt)
  | EBinary (l, BMod, r) -> compile_exp program type_defs env (ECall ("_mod", [l; r]), TInt)
  | EBinary (l, BEQ, r) -> compile_exp program type_defs env (ECall ("_eq", [l; r]), TBool)
  | EBinary (l, BNE, r) -> compile_exp program type_defs env (ECall ("_ne", [l; r]), TBool)
  | EBinary (l, BLT, r) -> compile_exp program type_defs env (ECall ("_lt", [l; r]), TBool)
  | EBinary (l, BGT, r) -> compile_exp program type_defs env (ECall ("_lt", [r; l]), TBool)
  | EBinary (l, BLE, r) -> compile_exp program type_defs env (ECall ("_ge", [r; l]), TBool)
  | EBinary (l, BGE, r) -> compile_exp program type_defs env (ECall ("_ge", [l; r]), TBool)
  | EBinary (l, BAnd, r) -> compile_exp program type_defs env (ECall ("_and", [l; r]), TBool)
  | EBinary (l, BOr, r) -> compile_exp program type_defs env (ECall ("_or", [l; r]), TBool)
  | ESet (n, v) ->
    compile_exp program type_defs env v;
    Program.add_instructions program [IPush];
    compile_lval program type_defs env n;
    Program.add_instructions program [IPopi]
  | EBreak e ->
    compile_exp program type_defs env e;
    Program.add_instructions program [IHalt]
  | EBool true -> Program.add_instructions program [ILoco (Int 1)]
  | EBool false -> Program.add_instructions program [ILoco (Int 0)]
  | EIf (c, t, e) ->
    let else_label = Program.new_label program in
    let end_label = Program.new_label program in 
    compile_exp program type_defs env c;
    Program.add_instructions program [IJzer (Label else_label)];
    compile_exp program type_defs env t;
    Program.add_instructions program [
      IJump (Label end_label);
      ILabel else_label;
    ];
    compile_exp program type_defs env e;
    Program.add_instructions program [ILabel end_label]
  | EWhile (c, b) ->
    let cond_label = Program.new_label program in
    let end_label = Program.new_label program in
    Program.add_instructions program [ILabel cond_label];
    compile_exp program type_defs env c;
    Program.add_instructions program [IJzer (Label end_label)];
    compile_exp program type_defs env b;
    Program.add_instructions program [
      IJump (Label cond_label);
      ILabel end_label;
    ]
  | EAs (e, _) -> compile_exp program type_defs env e
  | EUnit -> ()
  | EString s ->
    let label = Printf.sprintf "s%d" (String.hash s) in
    Program.add_constant program label (CString s);
    Program.add_instructions program [ILoco (Label label)]
  | EBlock [] -> ()
  | EBlock (e :: es) ->
    compile_exp program type_defs env e;
    compile_exp program type_defs env (EBlock es, type');
  | EIndex (e, i) ->
    compile_exp program type_defs env e;
    Program.add_instructions program [IPush];
    compile_exp program type_defs env i;
    Program.add_instructions program [
      IAddl 0;
      IInsp 1;
      IPshi;
      IPop;
    ]
  (* | EIndexSet (e, i, v) ->
    compile_exp program type_defs env v;
    Program.add_instructions program [IPush];
    compile_exp program type_defs env e;
    Program.add_instructions program [IPush];
    compile_exp program type_defs env i;
    Program.add_instructions program [
      IAddl 0;
      IInsp 1;
      IPopi;
    ] *)
  | EChar c -> Program.add_instructions program [ILoco (Char c)]
  | EStruct (n, vs) ->
    let fs = match Hashtbl.find type_defs n with
    | Type.TStruct fs -> fs
    | _ -> failwith (n ^ " is not a struct type (how wasn't this caught during type checking?)") in
    compile_exp program type_defs env (ECall ("alloc", [(EInt (List.length fs), TInt)]), TPtr TInt);
    Program.add_instructions program [IPush];
    List.iteri (fun i (f, _) ->
      compile_exp program type_defs env (List.assoc f vs);
      Program.add_instructions program [
        IPush;
        ILoco (Int i);
        IAddl 1;
        IPopi;
      ]) fs;
    Program.add_instructions program [IPop]
  | EDot ((ea, et), f) ->
    (match et with
    | TName n ->
      (match Hashtbl.find type_defs n with
      | TStruct fs ->
        compile_exp program type_defs env (ea, et);
        let o = find_first f (List.map fst fs) in
        (* Printf.printf "compile EDot: struct type = %s, field = %s, offset = %d\n" (Type.to_string et) f o; *)
        Program.add_instructions program [
          IPush;
          ILoco (Int o);
          IAddl 0;
          IInsp 1;
          IPshi;
          IPop;
        ]
      | _ -> failwith (n ^ " is not a struct type (this should have been caught during type checking)"))
    | t -> failwith (Type.to_string t ^ " is not a struct type, and not a name either"))
  | EArray es ->
    let label = Program.new_label program in
    Program.add_constant program label (CArray (List.map (fun (e, _) -> match e with
    | Exp.EInt i -> Program.CInt i
    | EChar c -> CChar c
    | EUnit | EBool false -> CInt 0
    | EBool true -> CInt 1
    | _ -> failwith "Invalid array value (try setting it with an index later)") es));
    Program.add_instructions program [ILoco (Label label)]
  | EAddrOf n -> compile_lval program type_defs env n
and compile_lval program type_defs env (exp, _) =
  match exp with
  | Exp.EVar n ->
    Program.add_instructions program (match Env.find_opt env n with
    | None -> [ILoco (Label n)]
    | Some i -> [ILoco (Int i); IAddd (Label "fp")])
  | EIndex (e, i) ->
    compile_exp program type_defs env e;
    Program.add_instructions program [IPush];
    compile_exp program type_defs env i;
    Program.add_instructions program [
      IAddl 0;
      IInsp 1;
    ]
  | EDot ((ea, et), f) ->
    (match et with
    | TName n ->
      (match Hashtbl.find type_defs n with
      | TStruct fs ->
        compile_exp program type_defs env (ea, et);
        let o = find_first f (List.map fst fs) in
        (* Printf.printf "compile EDot: struct type = %s, field = %s, offset = %d\n" (Type.to_string et) f o; *)
        Program.add_instructions program [
          IPush;
          ILoco (Int o);
          IAddl 0;
          IInsp 1;
        ]
      | _ -> failwith (n ^ " is not a struct type (this should have been caught during type checking)"))
    | t -> failwith (Type.to_string t ^ " is not a struct type, and not a name either"))
  | _ -> failwith "Not an lval, has no address"