type ident = string;;

type const = Bool of bool | Int of int;;

type operator =
  | Add of string
  | Sub of string
  | Mul of string
  | Mod of string
  | Div of string
  | Eq of string
  | Neq of string
  | And of string
  | Or of string
  | Lt of string
  | LtEq of string;;

type expr =
  | Const of const
  | Ident of ident
  | Exprs of expr * operator * expr;;

type statement =
  | Define of ident * expr
  | Assign of ident * expr
  | If of expr * statement * statement
  | While of expr * statement
  | Seq of statement * statement;;
