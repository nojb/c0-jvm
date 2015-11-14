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

open ClassFile

let get_const tbl c =
  if Hashtbl.mem tbl c then
    Hashtbl.find tbl c
  else begin
    let i = Hashtbl.length tbl + 1 in
    Hashtbl.add tbl c i;
    i
  end

let get_int tbl n =
  get_const tbl (CONSTANT_Integer n)

let get_utf8 tbl s =
  get_const tbl (CONSTANT_Utf8 s)

let get_class tbl name =
  get_const tbl (CONSTANT_Class (get_utf8 tbl name))

let get_constant_pool tbl =
  let lst = Hashtbl.fold (fun c i acc -> (c, i) :: acc) tbl [] in
  let lst = List.sort (fun (_, i) (_, j) -> Pervasives.compare i j) lst in
  Array.of_list (List.map fst lst)

external set_byte : string -> int -> int -> unit = "%string_safe_set"

let set_byte2 s idx n =
  set_byte s idx ((n land 0xFF00) lsl 8);
  set_byte s (idx+1) (n land 0xFF)

let get_system_exit tbl =
  let cls = get_class tbl "java/lang/System" in
  let name_and_type =
    get_const tbl (CONSTANT_NameAndType (get_utf8 tbl "exit", get_utf8 tbl "(I)V"))
  in
  get_const tbl (CONSTANT_Methodref (cls, name_and_type))

let write_inst tbl str idx inst =
  match inst with
  | Compile.Kiload (0 | 1 | 2 | 3 as n) ->
      set_byte str idx (26 + n)
  | Kiload n ->
      set_byte str idx 21;
      set_byte str (idx+1) n
  | Kistore (0 | 1 | 2 | 3 as n) ->
      set_byte str idx (59 + n)
  | Kistore n ->
      set_byte str idx 54;
      set_byte str (idx+1) n
  | Kldc (-1l | 0l | 1l | 2l | 3l | 4l | 5l as n)  ->
      set_byte str idx (3 + Int32.to_int n)
  | Kldc n when n = Int32.of_int (Int32.to_int n land 0xFF) ->
      set_byte str idx 16;
      set_byte str (idx+1) (Int32.to_int n)
  | Kldc n ->
      set_byte str idx 18;
      set_byte str (idx+1) (get_int tbl n)
  | Kiadd ->
      set_byte str idx 96
  | Kisub ->
      set_byte str idx 100
  | Kimul ->
      set_byte str idx 104
  | Kidiv ->
      set_byte str idx 108
  | Kirem ->
      set_byte str idx 112
  | Kineg ->
      set_byte str idx 116
  | Kpop ->
      set_byte str idx 87
  | Kpop2 ->
      set_byte str idx 88
  | Kdup ->
      set_byte str idx 89
  | Kdup2 ->
      set_byte str idx 92
  | Kdup_x1 ->
      set_byte str idx 90
  | Kdup2_x1 ->
      set_byte str idx 93
  | Kdup_x2 ->
      set_byte str idx 91
  | Kdup2_x2 ->
      set_byte str idx 94
  | Kswap ->
      set_byte str idx 95
  | Kireturn ->
      set_byte str idx 184; (* invokestatic *)
      set_byte2 str (idx+1) (get_system_exit tbl);
      set_byte str (idx+3) 177
  | Kpushlocal _ | Kpoplocal | Kline _ ->
      assert false

let get_code tbl code =
  let len = List.fold_left (fun acc i -> acc + Compile.instruction_size i) 0 code in
  let the_locals = ref [] in
  let str = String.make len '\000' in
  let rec loop locals lines i = function
    | [] -> !the_locals, List.rev lines, str
    | Compile.Kpushlocal (pos, id) :: rest ->
        let locals = (pos, id, i) :: locals in
        loop locals lines i rest
    | Kpoplocal :: rest ->
        let (pos, id, i0), locals = match locals with x :: y -> x, y | [] -> assert false in
        the_locals := (get_utf8 tbl id, pos, i0, i - i0) :: !the_locals;
        loop locals lines i rest
    | Kline lnum :: rest ->
        let lines = (lnum, i) :: lines in
        loop locals lines i rest
    | inst :: rest ->
        write_inst tbl str i inst;
        loop locals lines (i + Compile.instruction_size inst) rest
  in
  loop [] [] 0 code

let emit {Compile.source_file; max_locals; max_stack; code} =
  let tbl = Hashtbl.create 0 in
  let this_class = get_class tbl "Main" in
  let super_class = get_class tbl "java/lang/Object" in
  let locals, lines, code = get_code tbl code in
  let local_variables =
    Array.of_list (List.map (fun (name_index, index, start_pc, length) ->
        {
          start_pc;
          length;
          name_index;
          descriptor_index = get_utf8 tbl "I";
          index;
        }
      ) locals)
  in
  let line_numbers =
    Array.of_list (List.map (fun (line_number, start_pc) -> {start_pc; line_number}) lines)
  in
  let methods =
    [|
      {
        access_flags = [ACC_PUBLIC; ACC_STATIC; ACC_FINAL];
        name_index = get_utf8 tbl "main";
        descriptor_index = get_utf8 tbl "([Ljava/lang/String;)V";
        attributes =
          [|
            {
              attribute_name_index = get_utf8 tbl "Code";
              attribute_info =
                Code
                  {
                    max_stack;
                    max_locals;
                    code;
                    exception_table = [| |];
                    attributes = [| |];
                  };
            };
            {
              attribute_name_index = get_utf8 tbl "LocalVariableTable";
              attribute_info = LocalVariableTable local_variables;
            };
            {
              attribute_name_index = get_utf8 tbl "LineNumberTable";
              attribute_info = LineNumberTable line_numbers;
            }
          |];
      }
    |]
  in
  let source_file =
    {
      attribute_name_index = get_utf8 tbl "SourceFile";
      attribute_info = SourceFile (get_utf8 tbl source_file);
    }
  in
  let constant_pool = get_constant_pool tbl in
  {
    magic = 0xCAFEBABEl;
    minor_version = 0;
    major_version = 50;
    constant_pool;
    access_flags = [ACC_SYNTHETIC; ACC_FINAL];
    this_class;
    super_class;
    interfaces = [| |];
    fields = [| |];
    methods;
    attributes =
      [|
        source_file;
      |];
  }
