type 'a t =
| SProgram of 'a t list
| SFun of string * (string * Type.t) list * Type.t * (string * Type.t * 'a Exp.t) list * 'a Exp.t
| SVar of string * Type.t * 'a Exp.t
| SImport of string
| SSig of string * Type.t
| STypeDef of string * Type.t