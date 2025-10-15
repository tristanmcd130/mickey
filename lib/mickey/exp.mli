type 'a t = 'a exp * 'a
and 'a exp =
| EInt of int
| ECall of string * 'a t list
| EVar of string
| EUnary of unary_op * 'a t
| EBinary of 'a t * binary_op * 'a t
| ESet of string * 'a t
| EBreak of 'a t
| EBool of bool
| EIf of 'a t * 'a t * 'a t
| EWhile of 'a t * 'a t
| EAs of 'a t * Type.t
| EUnit
| EString of string
| EBlock of 'a t list
| EIndex of 'a t * 'a t
| EIndexSet of 'a t * 'a t * 'a t
| EChar of string (* string since it's unescaped; the assembler will escape it *)
| EStruct of string * (string * 'a t) list
| EDot of 'a t * string
| EArray of 'a t list
| EAddrOf of string
and unary_op =
| UNeg
| UNot
and binary_op =
| BAdd
| BSub
| BMul
| BDiv
| BMod
| BEQ
| BNE
| BGT
| BLT
| BGE
| BLE
| BAnd
| BOr