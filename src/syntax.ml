type top = Top of statement list

type statement =
  | Define of ident * expr
  | Assign of ident * expr
  | If of expr * statement list * statement list
  | While of expr * statement list

type expr =
  | Const of const
  | Ident of ident
  | Exprs of expr * operator * expr

type ident = Ident of string

type const = Bool of bool | Int of int

type operator =
  | Add of string
  | Sub of string
  | Mult of string
  | Mod of string
  | Eq of string
  | Neq of string
  | And of string
  | Or of string
  | Lt of string
  | LtEq of string
