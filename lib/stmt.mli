type t =
| SBlock of t list
| SGlobals of (string * Type.t) list * t
| SFun of string * (string * Type.t) list * Type.t * (string * Type.t) list * t
| SAssign of string * Exp.t
| SCall of string * Exp.t list
| SIf of Exp.t * t * t
| SWhile of Exp.t * t
| SReturn of Exp.t