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
| EWhile of t * t
| EAs of t * Type.t
| EAt of string
and unary_op =
| UNeg
| UNot
| UDeref
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
| BPtrSet