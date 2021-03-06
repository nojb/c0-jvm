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

type cp_info =
  | CONSTANT_Class of {name_index: int}
  | CONSTANT_Fieldref of {class_index: int; name_and_type_index: int}
  | CONSTANT_Methodref of {class_index: int; name_and_type_index: int}
  | CONSTANT_InterfaceMethodref of {class_index: int; name_and_type_index: int}
  | CONSTANT_String of {string_index: int}
  | CONSTANT_Integer of {bytes: int32}
  | CONSTANT_Float of {bytes: int32}
  | CONSTANT_Long of {high_bytes: int32; low_bytes: int32}
  | CONSTANT_Double of {high_bytes: int32; low_bytes: int32}
  | CONSTANT_NameAndType of {name_index: int; descriptor_index: int}
  | CONSTANT_Utf8 of {bytes: string}
  | CONSTANT_MethodHandle of {reference_kind: int; reference_index: int}
  | CONSTANT_MethodType of {descriptor_index: int}
  | CONSTANT_InvokeDynamic of {bootstrap_method_attr_index: int; name_and_type_index: int}

type access_flag =
  | ACC_PUBLIC
  | ACC_PRIVATE
  | ACC_PROTECTED
  | ACC_STATIC
  | ACC_FINAL
  | ACC_SUPER
  | ACC_VOLATILE
  | ACC_TRANSIENT
  | ACC_SYNTHETIC
  | ACC_ENUM

let int_of_flag = function
  | ACC_PUBLIC -> 0x0001
  | ACC_PRIVATE -> 0x0002
  | ACC_PROTECTED -> 0x0004
  | ACC_STATIC -> 0x0008
  | ACC_FINAL -> 0x0010
  | ACC_SUPER -> 0x0020
  | ACC_VOLATILE -> 0x0040
  | ACC_TRANSIENT -> 0x0080
  | ACC_SYNTHETIC -> 0x1000
  | ACC_ENUM -> 0x4000

type verification_type_info =
  | ITEM_Top
  | ITEM_Integer
  | ITEM_Float
  | ITEM_Long
  | ITEM_Double
  | ITEM_Null
  | ITEM_UninitializedThis
  | ITEM_Object of {cpool_index: int}
  | ITEM_Uninitialized of {offset: int}

type stack_map_frame =
  | SAME of {offset_delta: int}
  | SAME_LOCALS_1_STACK_ITEM of {offset_delta: int; stack: verification_type_info}
  | SAME_LOCALS_1_STACK_ITEM_EXTENDED of {offset_delta: int; stack: verification_type_info}
  | CHOP of {k: int; offset_delta: int}
  | SAME_FRAME_EXTENDED of {offset_delta: int}
  | APPEND of {k: int; offset_delta: int; locals: verification_type_info array}
  | FULL_FRAME of {offset_delta: int; locals: verification_type_info array; stack: verification_type_info array}

type exception_table_info =
  {
    start_pc: int;
    end_pc: int;
    handler_pc: int;
    catch_type: int;
  }

type local_variable_info =
  {
    start_pc: int;
    length: int;
    name_index: int;
    descriptor_index: int;
    index: int;
  }

type line_number_info =
  {
    start_pc: int;
    line_number: int;
  }

type attribute_info =
  | ConstantValue of {constant_value_index: int}
  | Code of {max_stack: int; max_locals: int; code: string; exception_table: exception_table_info array; attributes: attribute array}
  | LocalVariableTable of {local_variable_table: local_variable_info array}
  | LineNumberTable of {line_number_table: line_number_info array}
  | StackMapTable of {entries: stack_map_frame array}
  | Exceptions of {exception_index_table: int array}
  | SourceFile of {sourcefile_index: int}

and attribute =
  {
    attribute_name_index: int;
    attribute_info: attribute_info;
  }

let rec attribute_length = function
  | Code {code; exception_table; attributes; _} ->
      8 + String.length code + 2 +
      8 * Array.length exception_table +
      2 + Array.fold_left (fun acc {attribute_info; _} -> acc + 6 + attribute_length attribute_info) 0 attributes
  | SourceFile _ ->
      2
  | LocalVariableTable {local_variable_table} ->
      2 + 10 * Array.length local_variable_table
  | LineNumberTable {line_number_table} ->
      2 + 4 * Array.length line_number_table
  | _ ->
      failwith "attribute_length"

type field_info =
  {
    access_flags : access_flag list;
    name_index : int;
    descriptor_index : int;
    attributes : attribute array;
  }

type class_file =
  {
    magic: int32;
    minor_version: int;
    major_version: int;
    constant_pool: cp_info array;
    access_flags: access_flag list;
    this_class: int;
    super_class: int;
    interfaces: int array;
    fields: field_info array;
    methods: field_info array;
    attributes: attribute array;
  }

let output_int32 oc n =
  output_binary_int oc (Int32.to_int n) (* FIXME *)

let output_int16 oc n =
  output_byte oc ((n land 0xFF00) lsl 8);
  output_byte oc (n land 0xFF)

let output_constant oc = function
  | CONSTANT_Class {name_index} ->
      output_byte oc 7;
      output_int16 oc name_index
  | CONSTANT_Utf8 {bytes} ->
      output_byte oc 1;
      output_int16 oc (String.length bytes);
      output_string oc bytes (* FIXME Unicode *)
  | CONSTANT_Integer {bytes} ->
      output_byte oc 3;
      output_int32 oc bytes
  | CONSTANT_Methodref {class_index; name_and_type_index} ->
      output_byte oc 10;
      output_int16 oc class_index;
      output_int16 oc name_and_type_index
  | CONSTANT_NameAndType {name_index; descriptor_index} ->
      output_byte oc 12;
      output_int16 oc name_index;
      output_int16 oc descriptor_index
  | _ ->
      failwith "output_constant"

let output_flags oc flags =
  let n = List.fold_left (fun acc flag -> acc lor (int_of_flag flag)) 0 flags in
  output_int16 oc n

let output_field _oc = function
  | _ -> failwith "output_field"

let output_exception_table_info _oc = function
  | _ -> failwith "output_exception_table_info"

let output_local_variable_info oc (lv : local_variable_info) =
  output_int16 oc lv.start_pc;
  output_int16 oc lv.length;
  output_int16 oc lv.name_index;
  output_int16 oc lv.descriptor_index;
  output_int16 oc lv.index

let output_line_number_info oc (ln : line_number_info) =
  output_int16 oc ln.start_pc;
  output_int16 oc ln.line_number

let rec output_attribute_info oc = function
  | Code code ->
      output_int16 oc code.max_stack;
      output_int16 oc code.max_locals;
      output_int32 oc (Int32.of_int (String.length code.code));
      output_string oc code.code;
      output_int16 oc (Array.length code.exception_table);
      Array.iter (output_exception_table_info oc) code.exception_table;
      output_int16 oc (Array.length code.attributes);
      Array.iter (output_attribute oc) code.attributes
  | SourceFile {sourcefile_index} ->
      output_int16 oc sourcefile_index
  | LocalVariableTable {local_variable_table} ->
      output_int16 oc (Array.length local_variable_table);
      Array.iter (output_local_variable_info oc) local_variable_table;
  | LineNumberTable {line_number_table} ->
      output_int16 oc (Array.length line_number_table);
      Array.iter (output_line_number_info oc) line_number_table
  | _ ->
      failwith "output_attribute_info"

and output_attribute oc attr =
  output_int16 oc attr.attribute_name_index;
  output_int32 oc (Int32.of_int (attribute_length attr.attribute_info));
  output_attribute_info oc attr.attribute_info

let output_method oc (meth : field_info) =
  output_flags oc meth.access_flags;
  output_int16 oc meth.name_index;
  output_int16 oc meth.descriptor_index;
  output_int16 oc (Array.length meth.attributes);
  Array.iter (output_attribute oc) meth.attributes

let output_class oc cls =
  output_int32 oc cls.magic;
  output_int16 oc cls.minor_version;
  output_int16 oc cls.major_version;
  output_int16 oc (Array.length cls.constant_pool + 1);
  Array.iter (output_constant oc) cls.constant_pool;
  output_flags oc cls.access_flags;
  output_int16 oc cls.this_class;
  output_int16 oc cls.super_class;
  output_int16 oc (Array.length cls.interfaces);
  Array.iter (output_int16 oc) cls.interfaces;
  output_int16 oc (Array.length cls.fields);
  Array.iter (output_field oc) cls.fields;
  output_int16 oc (Array.length cls.methods);
  Array.iter (output_method oc) cls.methods;
  output_int16 oc (Array.length cls.attributes);
  Array.iter (output_attribute oc) cls.attributes
