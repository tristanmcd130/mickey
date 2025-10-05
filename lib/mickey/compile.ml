let rec compile program env = function
| Stmt.SProgram [] | SSig _ -> ()
| SProgram (s :: ss) ->
  compile program env s;
  compile program env (SProgram ss)
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
  List.iter (fun (n, _, v) -> compile_exp program local_env (Exp.ESet (n, v))) ls;
  compile_exp program local_env b;
  Program.add_instructions program [
    IStod (Label "tmp");
    IInsp (List.length ls);
    IPop;
    IStod (Label "fp");
    ILodd (Label "tmp");
    IRetn;
  ]
| SVar (n, _, v) ->
  (match v with
  | EInt i -> Program.add_constant program n (CInt i)
  | EString s -> Program.add_constant program n (CString s)
  | _ -> failwith "Invalid global initializer")
| SImport _ -> failwith "import still present even after preprocessing... and it's also not caught by type checking?"
and compile_exp program env = function
| Exp.EInt i -> Program.add_instructions program [ILoco (Int i)]
| ECall (n, a) ->
  List.rev a |> List.iter (fun x -> compile_exp program env x; Program.add_instructions program [IPush]);
  Program.add_instructions program [
    ICall (Label n);
    IInsp (List.length a);
  ]
| EVar n ->
  Program.add_instructions program (match Env.find_opt env n with
  | None -> [ILodd (Label n)]
  | Some i -> [ILoco (Int i); IAddd (Label "fp"); IPshi; IPop])
| EUnary (UNeg, r) ->
  compile_exp program env r;
  Program.add_instructions program [
    IPush;
    ILoco (Int 0);
    ISubl 0;
    IInsp 1;
  ]
| EUnary (UNot, r) ->
  compile_exp program env r;
  Program.add_instructions program [
    IPush;
    ILoco (Int 1);
    ISubl 0;
    IInsp 1;
  ]
| EUnary (UDeref, r) ->
  compile_exp program env r;
  Program.add_instructions program [
    IPshi;
    IPop;
  ]
| EBinary (l, BAdd, r) ->
  compile_exp program env r;
  Program.add_instructions program [IPush];
  compile_exp program env l;
  Program.add_instructions program [
    IAddl 0;
    IInsp 1;
  ]
| EBinary (l, BSub, r) ->
  compile_exp program env r;
  Program.add_instructions program [IPush];
  compile_exp program env l;
  Program.add_instructions program [
    ISubl 0;
    IInsp 1;
  ]
| EBinary (l, BMul, r) -> compile_exp program env (ECall ("mul", [l; r]))
| EBinary (l, BDiv, r) -> compile_exp program env (ECall ("div", [l; r]))
| EBinary (l, BEQ, r) -> compile_exp program env (ECall ("eq", [l; r]))
| EBinary (l, BNE, r) -> compile_exp program env (ECall ("ne", [l; r]))
| EBinary (l, BLT, r) -> compile_exp program env (ECall ("lt", [l; r]))
| EBinary (l, BGT, r) -> compile_exp program env (ECall ("lt", [r; l]))
| EBinary (l, BLE, r) -> compile_exp program env (ECall ("ge", [r; l]))
| EBinary (l, BGE, r) -> compile_exp program env (ECall ("ge", [l; r]))
| EBinary (l, BAnd, r) -> compile_exp program env (ECall ("and", [l; r]))
| EBinary (l, BOr, r) -> compile_exp program env (ECall ("or", [l; r]))
| EPtrSet (e, v) ->
  compile_exp program env v;
  Program.add_instructions program [IPush];
  compile_exp program env e;
  Program.add_instructions program [IPopi]
| ESet (n, v) ->
  compile_exp program env v;
  Program.add_instructions program (match Env.find_opt env n with
  | None -> [IStod (Label n)]
  | Some i -> [IPush; ILoco (Int i); IAddd (Label "fp"); IPopi])
| EBreak e ->
  compile_exp program env e;
  Program.add_instructions program [IHalt]
| EBool true -> Program.add_instructions program [ILoco (Int 1)]
| EBool false -> Program.add_instructions program [ILoco (Int 0)]
| EIf (c, t, e) ->
  let else_label = Program.new_label program in
  let end_label = Program.new_label program in 
  compile_exp program env c;
  Program.add_instructions program [IJzer (Label else_label)];
  compile_exp program env t;
  Program.add_instructions program [
    IJump (Label end_label);
    ILabel else_label;
  ];
  compile_exp program env e;
  Program.add_instructions program [ILabel end_label]
| EWhile (c, b) ->
  let cond_label = Program.new_label program in
  let end_label = Program.new_label program in
  Program.add_instructions program [ILabel cond_label];
  compile_exp program env c;
  Program.add_instructions program [IJzer (Label end_label)];
  compile_exp program env b;
  Program.add_instructions program [
    IJump (Label cond_label);
    ILabel end_label;
  ]
| EAs (e, _) -> compile_exp program env e
| EAddrOf n ->
  Program.add_instructions program (match Env.find_opt env n with
  | None -> [ILoco (Label n)]
  | Some i -> [ILoco (Int i); IAddd (Label "fp")])
| EUnit -> ()
| EString s ->
  let label = Printf.sprintf "s%d" (String.hash s) in
  Program.add_constant program label (CString s);
  Program.add_instructions program [ILoco (Label label)]
| EBlock [] -> ()
| EBlock (e :: es) ->
  compile_exp program env e;
  compile_exp program env (EBlock es);