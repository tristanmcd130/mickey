let rec compile stmt locals =
  match stmt with
  | Stmt.SBlock b -> String.concat "\n" (List.map (fun x -> compile x locals) b)
  | SGlobals (gs, b) -> compile b locals ^ "\ncall main:\nhalt\n" ^ (List.map fst gs |> List.map (fun x -> x ^ ": 0") |> String.concat "\n") 
  | SFun (n, ps, _, ls, b) -> n ^ ":\ndesp " ^ (List.length ls |> string_of_int) ^ "\n" ^ compile b (List.mapi (fun i (n, _) -> (n, i + List.length ls + 1)) ps @ List.mapi (fun i (n, _) -> (n, i)) ls @ locals) ^ "\ninsp " ^ (List.length ls |> string_of_int) ^ "\nloco 0\nretn"
  | _ -> "TODO"