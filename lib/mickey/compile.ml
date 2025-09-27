let rec compile program env = function
| Stmt.SProgram [] -> ()
| SProgram (s :: ss) ->
  compile program env s;
  compile program env (SProgram ss)
| SFun (n, ps, _, ls, b) ->
  (* Printf.sprintf "%s:\nlodd fp:\npush\ndesp %d\nswap\nstod fp:\nswap\n%sstod tmp:\ninsp %d\npop\nstod fp:\nlodd tmp:\nretn\n\n" n (List.length ls) (compile_exp (Env.create (Some env) (List.mapi (fun i (n, _) -> (n, i)) (ls @ [("", Type.TUnit); ("", TUnit)] @ ps))) b) (List.length ls) *)
  Program.add_instructions program [
    ILabel n;
    ILodd (Label "fp");
    IPush;
    IDesp (List.length ls);
    ISwap;
    IStod (Label "fp");
    ISwap;
  ];
  compile_exp program (Env.create (Some env) (List.mapi (fun i (n, _) -> (n, i)) (ls @ [("", Type.TUnit); ("", TUnit)] @ ps))) b;
  Program.add_instructions program [
    IStod (Label "tmp");
    IInsp (List.length ls);
    IPop;
    IStod (Label "fp");
    ILodd (Label "tmp");
    IRetn;
  ]
  (*
  arg 3
  arg 2
  arg 1
  return addr
  old fp
  local 3
  local 2
  local 1     <- fp
  temp 1
  temp 2
  temp 3      <- sp
  *)
| SVar (n, _, v) ->
  (match v with
  | EInt i -> Program.add_constant program n (CInt i)
  | EString s -> Program.add_constant program n (CString s)
  | _ -> failwith "Invalid global initializer")
and compile_exp program env = function
| Exp.EInt i -> Program.add_instructions program [ILoco (Int i)]
| ECall (n, a) ->
  (* (List.rev_map (fun x -> compile_exp env x ^ "push\n") a |> String.concat "") ^ Printf.sprintf "call %s:\ninsp %d\n" n (List.length a) *)
  List.rev a |> List.iter (fun x -> compile_exp program env x; Program.add_instructions program [IPush]);
  Program.add_instructions program [
    ICall (Label n);
    IInsp (List.length a);
  ]
| EVar n ->
  Program.add_instructions program (match Env.find_opt env n with
  | None -> [ILodd (Label n)]
  | Some i -> [ILoco (Int i); ICall (Label "getlocal")])
| EUnary (UNeg, r) ->
  (* compile_exp env r ^ "push\nloco 0\nsubl 0\ninsp 1\n" *)
  compile_exp program env r;
  Program.add_instructions program [
    IPush;
    ILoco (Int 0);
    ISubl 0;
    IInsp 1;
  ]
| EUnary (UNot, r) ->
  (* compile_exp env r ^ "push\nloco 1\nsubl 0\ninsp 1\n" *)
  compile_exp program env r;
  Program.add_instructions program [
    IPush;
    ILoco (Int 1);
    ISubl 0;
    IInsp 1;
  ]
| EUnary (UDeref, r) ->
  (* compile_exp env r ^ "pshi\npop\n" *)
  compile_exp program env r;
  Program.add_instructions program [
    IPshi;
    IPop;
  ]
| EBinary (l, BAdd, r) ->
  (* Printf.sprintf "%spush\n%saddl 0\ninsp 1\n" (compile_exp env r) (compile_exp env l) *)
  compile_exp program env r;
  Program.add_instructions program [IPush];
  compile_exp program env l;
  Program.add_instructions program [
    IAddl 0;
    IInsp 1;
  ]
| EBinary (l, BSub, r) ->
  (* Printf.sprintf "%spush\n%ssubl 0\ninsp 1\n" (compile_exp env r) (compile_exp env l) *)
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
| EBinary (l, BPtrSet, r) ->
  (* Printf.sprintf "%spush\n%spopi\n" (compile_exp env r) (compile_exp env l) *)
  compile_exp program env r;
  Program.add_instructions program [IPush];
  compile_exp program env l;
  Program.add_instructions program [IPopi]
| ESet (n, v) ->
  compile_exp program env v;
  Program.add_instructions program (match Env.find_opt env n with
  | None -> [IStod (Label n)]
  | Some i -> [IPush; ILoco (Int i); ICall (Label "setlocal"); IInsp 1])
| EBinary (l, BChain, r) ->
  compile_exp program env l;
  compile_exp program env r
| EBreak e ->
  compile_exp program env e;
  Program.add_instructions program [IHalt]
| EBool true -> Program.add_instructions program [ILoco (Int 1)]
| EBool false -> Program.add_instructions program [ILoco (Int 0)]
| EIf (c, t, e) ->
  let else_label = Program.new_label program in
  let end_label = Program.new_label program in 
  (* Printf.sprintf "%sjzer %s\n%sjump %s\n%s\n%s%s\ninsp 0 ; sadly mandatory nop instruction\n" (compile_exp env c) else_label (compile_exp env t) end_label else_label (compile_exp env e) end_label *)
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
  (* Printf.sprintf "%s\n%sjzer %s\n%sjump %s\n%s\ninsp 0 ; sadly mandatory nop instruction\n" cond_label (compile_exp env c) end_label (compile_exp env b) cond_label end_label *)
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