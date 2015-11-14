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

type 'a desc =
  {
    desc : 'a;
    loc : Lexing.position * Lexing.position;
  }

type binop =
  | Add | Sub | Mul | Div | Mod

type exp =
  | Const of int32
  | Ident of string
  | Binop of exp desc * binop * exp desc

type stmt =
  | Declare of string * exp desc option * stmt
  | Assign of string * exp desc
  | Nop
  | Seq of stmt * stmt
  | Return of exp desc

let rec seq s1 s2 =
  match s1, s2 with
  | Nop, _ -> s2
  | _, Nop -> s1
  | Seq (s, s'), _ -> seq s (seq s' s2)
  | _ -> Seq (s1, s2)

let line_num {loc = (startpos, _); _} =
  startpos.Lexing.pos_lnum
