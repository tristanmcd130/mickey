type 'a t = {
  parent: 'a t option;
  bindings: (string, 'a) Hashtbl.t;
}

let create parent bindings = {parent; bindings = bindings |> List.to_seq |> Hashtbl.of_seq}
let add env name value = Hashtbl.replace env.bindings name value
let rec find_opt env name =
   match Hashtbl.find_opt env.bindings name with
  | None ->
    (match env.parent with
    | None -> None
    | Some p -> find_opt p name)
  | Some v -> Some v
let find env name =
  match find_opt env name with
  | None -> failwith (name ^ " not defined")
  | Some v -> v