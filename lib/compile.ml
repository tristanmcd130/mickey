let label = ref 0

let rec compile stmt ctx = (compile_global_init stmt ctx, compile_globals stmt ctx, compile_stmt stmt ctx)
and compile_global_init stmt ctx =
  match stmt with
  | Stmt.SBlock b -> String.concat "\n" (List.map (fun x -> compile_global_init x ctx) b)
  | SImport f -> compile_global_init (In_channel.open_text f |> Lexing.from_channel |> Parser.prog Lexer.read) ctx
  | SGlobals (_, b) -> compile_stmt b ctx
  | _ -> ""
and compile_globals stmt ctx =
  match stmt with
  | Stmt.SBlock b -> String.concat "\n" (List.map (fun x -> compile_globals x ctx) b)
  | SImport f -> compile_globals (In_channel.open_text f |> Lexing.from_channel |> Parser.prog Lexer.read) ctx
  | SGlobals (gs, _) -> List.map fst gs |> List.map (fun x -> x ^ ": 0") |> String.concat "\n"
  | _ -> ""
and compile_stmt stmt ctx =
  match stmt with
  | Stmt.SBlock b -> String.concat "\n" (List.map (fun x -> compile_stmt x ctx) b)
  | SImport f -> compile_stmt (In_channel.open_text f |> Lexing.from_channel |> Parser.prog Lexer.read) ctx
  | SGlobals (_, _) -> ""
  | SFun (n, ps, _, ls, b) -> n ^ ":\nlodd fp:\npush\nlodd lp:\npush\nswap\nstod fp:\nswap\ndesp " ^ (List.length ls |> string_of_int) ^ "\nswap\nstod lp:\nswap\n" ^ compile_stmt b (List.mapi (fun i (n, _) -> (n, i + List.length ls + 3)) ps @ List.mapi (fun i (n, _) -> (n, i)) ls @ ctx) ^ "\nhalt ; shouldnt be able to get here, but just in case\n"
  | SAssign (n, v) ->
    compile_exp v ctx ^ "\n" ^ (match List.assoc_opt n ctx with
    | Some o -> "push\nloco " ^ string_of_int o ^ "\ncall setlocal:\ninsp 1"
    | None -> "stod " ^ n ^ ":")
  | SPtrAssign (p, v) -> compile_exp v ctx ^ "\npush\n" ^ compile_exp p ctx ^ "\npopi"
  | SCall (n, a) -> (List.rev_map (fun x -> compile_exp x ctx ^ "\npush") a |> String.concat "\n") ^ "\ncall " ^ n ^ ":\ninsp " ^ (List.length a |> string_of_int)
  | SIf (c, t, e) ->
    let then_label = "if" ^ (!label |> string_of_int) ^ ":" in
    let done_label = "if" ^ (!label + 1 |> string_of_int) ^ ":" in
    label := !label + 2;
    compile_exp c ctx ^ "\njnze " ^ then_label ^ "\n" ^ compile_stmt e ctx ^ "\njump " ^ done_label ^ "\n" ^ then_label ^ "\n" ^ compile_stmt t ctx ^ "\n" ^ done_label
  | SWhile (c, b) ->
    let do_label = "while" ^ (!label |> string_of_int) ^ ":" in
    let done_label = "while" ^ (!label + 1 |> string_of_int) ^ ":" in
    label := !label + 2;
    do_label ^ "\n" ^ compile_exp c ctx ^ "\njzer " ^ done_label ^ "\n" ^ compile_stmt b ctx ^ "\njump " ^ do_label ^ "\n" ^ done_label
  | SReturn e -> (match e with Some v -> compile_exp v ctx | None -> "") ^ "\nstod tmp:\nlodd fp:\nswap\npop\nstod lp:\npop\nstod fp:\nlodd tmp:\nretn"
  | SAsm s -> s
and compile_exp exp ctx =
  match exp with
  | EBool true -> "loco 1"
  | EBool false -> "loco 0"
  | EInt i -> 
    if 0 <= i && i < 4096 then
      "loco " ^ string_of_int i
    else
      let c_label = "c" ^ (i |> string_of_int |> String.map (fun x -> if x = '-' then 'n' else x)) ^ ":" in
      let cjump_label = "cjump" ^ (!label |> string_of_int) ^ ":" in
      label := !label + 1;
      "lodd " ^ c_label ^ "\njump " ^ cjump_label ^ "\n" ^ c_label ^ " " ^ string_of_int i ^ "\n" ^ cjump_label
  | EString s -> ""
  | EVar v ->
    (match List.assoc_opt v ctx with
    | Some o -> "loco " ^ string_of_int o ^ "\ncall getlocal:"
    | None -> "lodd " ^ v ^ ":")
  | ECall (n, a) -> (List.rev_map (fun x -> compile_exp x ctx ^ "\npush") a |> String.concat "\n") ^ "\ncall " ^ n ^ ":\ninsp " ^ (List.length a |> string_of_int)
  | EAs (e, _) -> compile_exp e ctx
  | EAt p -> compile_exp p ctx ^ "\npshi\npop"