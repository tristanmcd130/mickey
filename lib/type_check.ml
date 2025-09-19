let rec type_check scope = function
| Ast.AProgram (s :: ss) ->
  type_check scope s;
  type_check scope (AProgram ss)
| AFun (n, ps, t, b) ->
  let local_scope = Scope.create (Some scope) ps in
  assert (type_of local_scope b = t);
  Scope.add scope n (Type.TArrow (List.map snd ps, t))
| _ -> ()
and type_of scope = function
| AInt i -> TInt
| ABlock [e] -> type_of scope e
| ABlock (_ :: es) -> type_of scope (ABlock es)
| _ -> TUnit