(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

type label = Label of int

type stack_type_info =
  | Tint
  | Tfloat
  | Tlong
  | Tdouble
  | Tnull
  | Tobject of string
  | Tuninitialized of label
  | Tuninitialized_this

type stack_frame =
  | Fsame
  | Fsingle of stack_type_info
  | Fchop of int
  | Fappend of stack_type_info list

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
  | Kpushlocal of (int * string)
  | Kpoplocal
  | Kline of int
  | Kstackframe of stack_frame
  | Klabel of label

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
  | Kdup2_x1 | Kdup2_x2 | Kswap ->
      1
  | Kpushlocal _ | Kpoplocal | Kline _ | Kstackframe _ | Klabel _ ->
      0
  | Kireturn ->
      4

open Ast

let compile_binop = function
  | Add -> Kiadd
  | Sub -> Kisub
  | Mul -> Kimul
  | Div -> Kidiv
  | Mod -> Kirem

module M = Map.Make (String)

let max_stack = ref (-1)
let last_line = ref (-1)

let rec compile_exp sz env exp cont =
  if sz > !max_stack then max_stack := sz;
  let cont =
    match exp.desc with
    | Const n ->
        Kldc n :: cont
    | Ident id ->
        let pos = M.find id env in
        Kiload pos :: cont
    | Binop (e1, op, e2) ->
        compile_exp sz env e1 (compile_exp (sz+1) env e2 (compile_binop op :: cont))
  in
  let lnum = line_num exp in
  if lnum <> !last_line then begin
    last_line := lnum;
    Kline lnum :: cont
  end else
    cont

let max_locals = ref (-1)

let rec compile_stmt loc env stmt cont =
  if loc > !max_locals then max_locals := loc;
  match stmt with
  | Declare (id, None, stmt) ->
      compile_stmt (loc+1) (M.add id loc env) stmt cont
  | Declare (id, Some e, stmt) ->
      let cont = compile_stmt (loc+1) (M.add id loc env) stmt (Kpoplocal :: cont) in
      compile_exp 0 env e (Kistore loc :: Kpushlocal (loc, id) :: cont)
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
    source_file : string;
    max_locals : int;
    max_stack : int;
    code : instruction list;
  }

let compile source_file prog =
  max_locals := 0;
  max_stack := 0;
  let code = compile_stmt 0 M.empty prog [] in
  let max_locals = !max_locals + 1 in
  let max_stack = !max_stack + 1 in
  {source_file; max_locals; max_stack; code}
