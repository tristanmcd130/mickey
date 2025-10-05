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
| IString of string
| IChar of string
and arg =
| Int of int
| Label of string
| Char of string

val to_string: t -> string