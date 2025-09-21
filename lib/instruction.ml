type t =
| ILodd of arg
| IStod of arg
| IAddd of arg
| ISubd of arg
| IJpos of arg
| IJzer of arg
| IJump of arg
| ILoco of arg
| ILodl of int
| IStol of int
| IAddl of int
| ISubl of int
| IJneg of arg
| IJnze of arg
| ICall of arg
| IPshi
| IPopi
| IPush
| IPop
| IRetn
| ISwap
| IInsp of int
| IDesp of int
| IHalt
| ILabel of string
and arg =
| Int of int
| Label of string

let arg_to_string = function
| Int i -> string_of_int i
| Label l -> l ^ ":"
let to_string = function
| ILodd a -> Printf.sprintf "lodd %s" (arg_to_string a)
| IStod a -> Printf.sprintf "stod %s" (arg_to_string a)
| IAddd a -> Printf.sprintf "addd %s" (arg_to_string a)
| ISubd a -> Printf.sprintf "subd %s" (arg_to_string a)
| IJpos a -> Printf.sprintf "jpos %s" (arg_to_string a)
| IJzer a -> Printf.sprintf "jzer %s" (arg_to_string a)
| IJump a -> Printf.sprintf "jump %s" (arg_to_string a)
| ILoco a -> Printf.sprintf "loco %s" (arg_to_string a)
| ILodl a -> Printf.sprintf "lodl %d" a
| IStol a -> Printf.sprintf "stol %d" a
| IAddl a -> Printf.sprintf "addl %d" a
| ISubl a -> Printf.sprintf "subl %d" a
| IJneg a -> Printf.sprintf "jneg %s" (arg_to_string a)
| IJnze a -> Printf.sprintf "jnze %s" (arg_to_string a)
| ICall a -> Printf.sprintf "call %s" (arg_to_string a)
| IPshi -> "pshi"
| IPopi -> "popi"
| IPush -> "push"
| IPop -> "pop"
| IRetn -> "retn"
| ISwap -> "swap"
| IInsp a -> Printf.sprintf "insp %d" a
| IDesp a -> Printf.sprintf "desp %d" a
| IHalt -> "halt"
| ILabel l -> "\n" ^ l ^ ":"