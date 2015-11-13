type binop =
  | Add | Sub | Mul | Div | Mod

type exp =
  | Const of int32
  | Ident of string
  | Binop of exp * binop * exp

type stmt =
  | Declare of string * exp option * stmt
  | Assign of string * exp
  | Nop
  | Seq of stmt * stmt
  | Return of exp

let rec seq s1 s2 =
  match s1, s2 with
  | Nop, _ -> s2
  | _, Nop -> s1
  | Seq (s, s'), _ -> seq s (seq s' s2)
  | _ -> Seq (s1, s2)
