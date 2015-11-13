type instruction =
  | Kiload of int
  | Kistore of int
  | Kldc of int32
  | Kiadd
  | Kisub
  | Kimul
  | Kidiv
  | Kirem
  | Kineg
  | Kpop
  | Kpop2
  | Kdup
  | Kdup2
  | Kdup_x1
  | Kdup2_x1
  | Kdup_x2
  | Kdup2_x2
  | Kswap
  | Kireturn

let instruction_size = function
  | Kiload (0 | 1 | 2 | 3)
  | Kistore (0 | 1 | 2 | 3) ->
      1
  | Kiload _
  | Kistore _ -> (* FIXME wide *)
      2
  | Kldc (-1l | 0l | 1l | 2l | 3l | 4l | 5l) ->
      1
  | Kldc _ -> (* FIXME wide *)
      2
  | Kiadd | Kisub | Kimul | Kidiv | Kirem | Kineg
  | Kpop | Kpop2 | Kdup | Kdup2 | Kdup_x1 | Kdup_x2
  | Kdup2_x1 | Kdup2_x2 | Kswap | Kireturn ->
      1

open Ast

let compile_binop = function
  | Add -> Kiadd
  | Sub -> Kisub
  | Mul -> Kimul
  | Div -> Kidiv
  | Mod -> Kirem

module M = Map.Make (String)

let max_stack = ref 0

let rec compile_exp sz env exp cont =
  if sz > !max_stack then max_stack := sz;
  match exp with
  | Const n ->
      Kldc n :: cont
  | Ident id ->
      let pos = M.find id env in
      Kiload pos :: cont
  | Binop (e1, op, e2) ->
      compile_exp sz env e1 (compile_exp (sz+1) env e2 (compile_binop op :: cont))

let max_locals = ref 0

let rec compile_stmt loc env stmt cont =
  if loc > !max_locals then max_locals := loc;
  match stmt with
  | Declare (id, None, stmt) ->
      compile_stmt (loc+1) (M.add id loc env) stmt cont
  | Declare (id, Some e, stmt) ->
      let cont = compile_stmt (loc+1) (M.add id loc env) stmt cont in
      compile_exp 0 env e (Kistore loc :: cont)
  | Assign (id, e) ->
      let loc = M.find id env in
      compile_exp 0 env e (Kistore loc :: cont)
  | Nop ->
      cont
  | Seq (s1, s2) ->
      compile_stmt loc env s1 (compile_stmt loc env s2 cont)
  | Return e ->
      compile_exp 0 env e (Kireturn :: cont)

type program =
  {
    max_locals : int;
    max_stack : int;
    code : instruction list;
  }

let compile prog =
  max_locals := 0;
  max_stack := 0;
  let code = compile_stmt 0 M.empty prog [] in
  let max_locals = !max_locals in
  let max_stack = !max_stack in
  {max_locals; max_stack; code}
