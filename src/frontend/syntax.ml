type ident = string
[@@deriving show]

type const = Bool of bool | Int of int
[@@deriving show]

type operator =
  | Add
  | Sub
  | Mul
  | Mod
  | Div
  | Eq 
  | Neq
  | Not
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
  | If of expr * statement * statement option
  | While of expr * statement
  | Seq of statement * statement
  | Emp
[@@deriving show]
