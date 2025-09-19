let rec compile = function
| Ast.AProgram [] -> ""
| AProgram (s :: ss) -> compile s ^ compile (AProgram ss)
| AFun (n, ps, _, b) -> Printf.sprintf "%s:\n%sretn" n (compile b)
| AInt i -> Printf.sprintf "loco %d\n" i
| ABlock [] -> "loco 0\n"
| ABlock [e] -> compile e
| ABlock (e :: es) -> compile e ^ compile (ABlock es)