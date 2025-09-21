let label = ref 0
let new_label () =
  let l = !label in
  label := !label + 1;
  Printf.sprintf "l%d:" l

let rec compile env = function
| Stmt.SProgram [] -> ""
| SProgram (s :: ss) -> compile env s ^ compile env (SProgram ss)
| SFun (n, ps, _, ls, b) -> Printf.sprintf "%s:\nlodd fp:\npush\ndesp %d\nswap\nstod fp:\nswap\n%sstod tmp:\ninsp %d\npop\nstod fp:\nlodd tmp:\nretn\n\n" n (List.length ls) (compile_exp (Env.create (Some env) (List.mapi (fun i (n, _) -> (n, i)) (ls @ [("", Type.TUnit); ("", TUnit)] @ ps))) b) (List.length ls)
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
| SVar (n, _) -> n ^ ": 0\n\n"
and compile_exp env = function
| EInt i -> Printf.sprintf "loco %d\n" i
| EBlock [] -> ""
| EBlock [e] -> compile_exp env e
| EBlock (e :: es) -> compile_exp env e ^ compile_exp env (EBlock es)
| ECall (n, a) -> (List.rev_map (fun x -> compile_exp env x ^ "push\n") a |> String.concat "") ^ Printf.sprintf "call %s:\ninsp %d\n" n (List.length a)
| EVar n ->
	(match Env.find_opt env n with
  | None -> Printf.sprintf "lodd %s:\n" n
  | Some i -> Printf.sprintf "loco %d\ncall getlocal:\n" i)
| EUnary (UNeg, r) -> compile_exp env r ^ "push\nloco 0\nsubl 0\ninsp 1\n"
| EUnary (UNot, r) -> compile_exp env r ^ "push\nloco 1\nsubl 0\ninsp 1\n"
| EUnary (UDeref, r) -> compile_exp env r ^ "pshi\npop\n"
| EBinary (l, BAdd, r) -> Printf.sprintf "%spush\n%saddl 0\ninsp 1\n" (compile_exp env r) (compile_exp env l)
| EBinary (l, BSub, r) -> Printf.sprintf "%spush\n%ssubl 0\ninsp 1\n" (compile_exp env r) (compile_exp env l)
| EBinary (l, BMul, r) -> compile_exp env (ECall ("mul", [l; r]))
| EBinary (l, BDiv, r) -> compile_exp env (ECall ("div", [l; r]))
| EBinary (l, BEQ, r) -> compile_exp env (ECall ("eq", [l; r]))
| EBinary (l, BNE, r) -> compile_exp env (ECall ("ne", [l; r]))
| EBinary (l, BLT, r) -> compile_exp env (ECall ("lt", [l; r]))
| EBinary (l, BGT, r) -> Printf.sprintf "%spush\n%spush\ncall lt:\ninsp 2\n" (compile_exp env l) (compile_exp env r)
| EBinary (l, BLE, r) -> Printf.sprintf "%spush\n%spush\ncall ge:\ninsp 2\n" (compile_exp env l) (compile_exp env r)
| EBinary (l, BGE, r) -> compile_exp env (ECall ("ge", [l; r]))
| EBinary (l, BAnd, r) -> compile_exp env (ECall ("and", [l; r]))
| EBinary (l, BOr, r) -> compile_exp env (ECall ("or", [l; r]))
| EBinary (l, BPtrSet, r) -> Printf.sprintf "%spush\n%spopi\n" (compile_exp env r) (compile_exp env l)
| ESet (n, v) -> compile_exp env v ^ (match Env.find_opt env n with
  | None -> Printf.sprintf "stod %s:\n" n
  | Some i -> Printf.sprintf "push\nloco %d\ncall setlocal:\ninsp 1\n" i)
| EBreak e -> compile_exp env e ^ "halt ; BREAK\n"
| EBool true -> "loco 1\n"
| EBool false -> "loco 0\n"
| EIf (c, t, e) ->
  let else_label = new_label () in
  let end_label = new_label () in 
  Printf.sprintf "%sjzer %s\n%sjump %s\n%s\n%s%s\ninsp 0 ; sadly mandatory nop instruction\n" (compile_exp env c) else_label (compile_exp env t) end_label else_label (compile_exp env e) end_label
| EWhile (c, b) ->
  let cond_label = new_label () in
  let end_label = new_label () in
  Printf.sprintf "%s\n%sjzer %s\n%sjump %s\n%s\ninsp 0 ; sadly mandatory nop instruction\n" cond_label (compile_exp env c) end_label (compile_exp env b) cond_label end_label
| EAs (e, _) -> compile_exp env e
| EAt n ->
  (match Env.find_opt env n with
  | None -> Printf.sprintf "loco %s:\n" n
  | Some i -> Printf.sprintf "loco %d\naddd fp:\n" i)