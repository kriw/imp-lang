type ident = string
[@@deriving show]

type boolean = True | False
[@@deriving show]

type const = Bool of boolean | Int of string
[@@deriving show]

type operator =
  | Add
  | Sub
  | Mul
  | Mod
  | Div
  | Eq 
  | Neq
  | And
  | Or 
  | Lt
  | LtEq
[@@deriving show]

type expr =
  | Const of const
  | Ident of string
  | Exprs of expr * operator * expr
[@@deriving show]

type statement =
  | Define of ident * expr
  | Assign of ident * expr
  | If of expr * statement * statement
  | While of expr * statement
  | Seq of statement * statement
[@@deriving show]
