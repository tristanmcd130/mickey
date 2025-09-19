type 'a t = {
  parent: 'a t option;
  bindings: (string, 'a) Hashtbl.t;
}

let create parent bindings = {parent; bindings = bindings |> List.to_seq |> Hashtbl.of_seq}
let add scope name value = Hashtbl.replace scope.bindings name value
let rec find scope name =
  match Hashtbl.find_opt scope.bindings name with
  | None ->
    (match scope.parent with
    | None -> failwith (name ^ " not defined")
    | Some p -> find p name)
  | Some v -> v