type t =
| EInt of int
| EBlock of t list
| ECall of string * t list
| EVar of string
| EUnary of unary_op * t
| EBinary of t * binary_op * t
| ESet of string * t
| EBreak of t
| EBool of bool
| EIf of t * t * t
and unary_op =
| UNeg
| UNot
and binary_op =
| BAdd
| BSub
| BMul
| BDiv
| BEQ
| BNE
| BGT
| BLT
| BGE
| BLE
| BAnd
| BOr