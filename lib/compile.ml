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
| EBlock [] -> "loco 0\n"
| EBlock [e] -> compile_exp env e
| EBlock (e :: es) -> compile_exp env e ^ compile_exp env (EBlock es)
| ECall (n, a) -> (List.rev_map (fun x -> compile_exp env x ^ "push\n") a |> String.concat "") ^ Printf.sprintf "call %s:\ninsp %d\n" n (List.length a)
| EVar n ->
	(match Env.find_opt env n with
  | None -> Printf.sprintf "lodd %s:\n" n
  | Some i -> Printf.sprintf "loco %d\ncall getlocal:\n" i)
| EUnary (o, r) -> compile_exp env (ECall ((match o with
  | UNeg -> "neg"
  | UNot -> "not"), [r]))
| EBinary (l, o, r) -> compile_exp env (ECall ((match o with
  | BAdd -> "add"
  | BSub -> "sub"
  | BMul -> "mul"
  | BDiv -> "div"
  | BEQ -> "eq"
  | BNE -> "ne"
  | BGT -> "gt"
  | BLT -> "lt"
  | BGE -> "ge"
  | BLE -> "le"
  | BAnd -> "and"
  | BOr -> "or"), [l; r]))
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