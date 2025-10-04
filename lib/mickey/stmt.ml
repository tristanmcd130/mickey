type t =
| SProgram of t list
| SFun of string * (string * Type.t) list * Type.t * (string * Type.t * Exp.t) list * Exp.t
| SVar of string * Type.t * Exp.t
| SImport of string
| SSig of string * Type.t