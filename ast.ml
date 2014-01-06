type expr =
  | ASTNum of int
  | ASTBool of bool
  | ASTString of string
  | ASTIdent of string
  | ASTRef of expr * string
  | ASTAff of expr * expr
  | ASTPlus of expr * expr
  | ASTMinus of expr * expr
  | ASTStar of expr * expr
  | ASTInf of expr * expr
  | ASTSup of expr * expr
  | ASTIf of expr * expr * expr
  | ASTLet of string * expr
  | ASTApp of string * expr list
  | ASTNew of expr
  | ASTAppMethod of expr * string * expr list
  | ASTArr of expr
  | ASTAdd of expr * expr * expr
  | ASTGetArray of expr * expr
  | ASTFor of string * expr * expr * expr list
;;

type class_declaration =
  | ASTField of string
  | ASTMethod of string * string list * expr list
;;

type statements =
  | ASTEval of expr
  | ASTVar of string * expr
  | ASTFun of string * string list * expr list
  | ASTClass of string * class_declaration list
;;

type progra =
  | ASTProg of statements list
;;
