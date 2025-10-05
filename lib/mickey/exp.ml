type t =
| EInt of int
| ECall of string * t list
| EVar of string
| EUnary of unary_op * t
| EBinary of t * binary_op * t
| ESet of string * t
| EPtrSet of t * t
| EBreak of t
| EBool of bool
| EIf of t * t * t
| EWhile of t * t
| EAs of t * Type.t
| EAddrOf of string
| EUnit
| EString of string
| EBlock of t list
| EIndex of t * t
| EIndexSet of t * t * t
| EChar of string (* string since it's unescaped; the assembler will escape it *)
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