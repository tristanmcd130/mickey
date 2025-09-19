let rec compile env = function
| Stmt.SProgram [] -> ""
| SProgram (s :: ss) -> compile env s ^ compile env (SProgram ss)
| SFun (n, ps, _, ls, b) -> Printf.sprintf "%s:\nlodd fp:\npush\ndesp %d\nswap\nstod fp:\nswap\n%sstod tmp:\ninsp %d\npop\nstod fp:\nlodd tmp:\nretn\n" n (List.length ls) (compile_exp (Env.create (Some env) (List.mapi (fun i (n, _) -> (n, i)) (ls @ [("", Type.TUnit); ("", TUnit)] @ ps))) b) (List.length ls)
and compile_exp env = function
| EInt i -> Printf.sprintf "loco %d\n" i
| EBlock [] -> "loco 0\n"
| EBlock [e] -> compile_exp env e
| EBlock (e :: es) -> compile_exp env e ^ compile_exp env (EBlock es)
| ECall (n, a) -> (List.rev_map (fun x -> compile_exp env x ^ "push\n") a |> String.concat "") ^ Printf.sprintf "call %s:\ninsp %d\n" n (List.length a)
| EVar n ->
	(match Env.find_opt env n with
  | None -> Printf.sprintf "lodd %s:\n" n
  | Some i -> Printf.sprintf "loco %d\ncall getlocal:\n" i)
| EUnary (o, r) -> compile_exp env (ECall ((match o with
  | UNeg -> "neg"), [r]))
| EBinary (l, o, r) -> compile_exp env (ECall ((match o with
  | BAdd -> "add"
  | BSub -> "sub"), [l; r]))
| ESet (n, v) -> compile_exp env v ^ (match Env.find_opt env n with
  | None -> Printf.sprintf "stod %s:\n" n
  | Some i -> Printf.sprintf "push\nloco %d\ncall setlocal:\ninsp 1\n" i)
| EBreak e -> compile_exp env e ^ "halt\t; BREAK\n"
| EBool true -> "loco 1\n"
| EBool false -> "loco 0\n"