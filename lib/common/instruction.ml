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
| IInt of int
and arg =
| Int of int
| Label of string

let arg_to_string = function
| Int i -> string_of_int i
| Label l -> l
let to_string = function
| ILodd a -> "lodd " ^ arg_to_string a
| IStod a -> "stod " ^ arg_to_string a
| IAddd a -> "addd " ^ arg_to_string a
| ISubd a -> "subd " ^ arg_to_string a
| IJpos a -> "jpos " ^ arg_to_string a
| IJzer a -> "jzer " ^ arg_to_string a
| IJump a -> "jump " ^ arg_to_string a
| ILoco a -> "loco " ^ arg_to_string a
| ILodl a -> "lodl " ^ string_of_int a
| IStol a -> "stol " ^ string_of_int a
| IAddl a -> "addl " ^ string_of_int a
| ISubl a -> "subl " ^ string_of_int a
| IJneg a -> "jneg " ^ arg_to_string a
| IJnze a -> "jnze " ^ arg_to_string a
| ICall a -> "call " ^ arg_to_string a
| IPshi -> "pshi"
| IPopi -> "popi"
| IPush -> "push"
| IPop -> "pop"
| IRetn -> "retn"
| ISwap -> "swap"
| IInsp a -> "insp " ^ string_of_int a
| IDesp a -> "desp " ^ string_of_int a
| IHalt -> "halt"
| ILabel l -> l ^ ":"
| IInt i -> string_of_int i