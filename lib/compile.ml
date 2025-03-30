let label = ref 0

let rec compile stmt ctx =
  match stmt with
  | Stmt.SBlock b -> String.concat "\n" (List.map (fun x -> compile x ctx) b)
  | SGlobals (gs, b) -> compile b ctx ^ "\n" ^ (List.map fst gs |> List.map (fun x -> x ^ ": 0") |> String.concat "\n") 
  | SFun (n, ps, _, ls, b) -> n ^ ":\nlodd fp:\npush\nlodd lp:\npush\nswap\nstod fp:\nswap\ndesp " ^ (List.length ls |> string_of_int) ^ "\nswap\nstod lp:\nswap\n" ^ compile b (List.mapi (fun i (n, _) -> (n, i + List.length ls + 3)) ps @ List.mapi (fun i (n, _) -> (n, i)) ls @ ctx) ^ "\nhalt"
  | SAssign (n, v) ->
    compile_exp v ctx ^ "\n" ^ (match List.assoc_opt n ctx with
    | Some o -> "push\nloco " ^ string_of_int o ^ "\ncall setlocal:\ninsp 1"
    | None -> "stod " ^ n ^ ":")
  | SCall (n, a) -> (List.rev_map (fun x -> compile_exp x ctx ^ "\npush") a |> String.concat "\n") ^ "\ncall " ^ n ^ ":\ninsp " ^ (List.length a |> string_of_int)
  | SIf (c, t, e) ->
    let then_label = "if" ^ (!label |> string_of_int) ^ ":" in
    let done_label = "if" ^ (!label + 1 |> string_of_int) ^ ":" in
    label := !label + 2;
    compile_exp c ctx ^ "\njnze " ^ then_label ^ "\n" ^ compile e ctx ^ "\njump " ^ done_label ^ "\n" ^ then_label ^ "\n" ^ compile t ctx ^ "\n" ^ done_label
  | SWhile (c, b) ->
    let do_label = "while" ^ (!label |> string_of_int) ^ ":" in
    let done_label = "while" ^ (!label + 1 |> string_of_int) ^ ":" in
    label := !label + 2;
    do_label ^ "\n" ^ compile_exp c ctx ^ "\njzer " ^ done_label ^ "\n" ^ compile b ctx ^ "\njump " ^ do_label ^ "\n" ^ done_label
  | SReturn e -> (match e with Some v -> compile_exp v ctx | None -> "") ^ "\nstod tmp:\nlodd fp:\nswap\npop\nstod lp:\npop\nstod fp:\nlodd tmp:\nretn"
and compile_exp exp ctx =
  match exp with
  | EBool true -> "loco 1"
  | EBool false -> "loco 0"
  | EInt i -> "loco " ^ string_of_int i
  | EVar v ->
    (match List.assoc_opt v ctx with
    | Some o -> "loco " ^ string_of_int o ^ "\ncall getlocal:"
    | None -> "lodd " ^ v ^ ":")
  | ECall (n, a) -> (List.rev_map (fun x -> compile_exp x ctx ^ "\npush") a |> String.concat "\n") ^ "\ncall " ^ n ^ ":\ninsp " ^ (List.length a |> string_of_int)