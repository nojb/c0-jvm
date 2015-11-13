module ClassFile = struct
  type cp_info =
    | CONSTANT_Class of int (* name_index *)
    | CONSTANT_Fieldref of int * int (* class_index, name_and_type_index *)
    | CONSTANT_Methodref of int * int (* class_index, name_and_type_index *)
    | CONSTANT_InterfaceMethodref of int * int (* class_index, name_and_type_index *)
    | CONSTANT_String of int (* string_index *)
    | CONSTANT_Integer of int32 (* bytes *)
    | CONSTANT_Float of int32 (* bytes, float ? *)
    | CONSTANT_Long of int64 (* high_bytes, low_bytes *)
    | CONSTANT_Double of float (* high_bytes, low_bytes *)
    | CONSTANT_NameAndType of int * int (* name_index, descriptor_index *)
    | CONSTANT_Utf8 of string (* bytes *)
    | CONSTANT_MethodHandle of int * int (* reference_kind, refernece_index *)
    | CONSTANT_MethodType of int (* descriptor_index *)
    | CONSTANT_InvokeDynamic of int * int (* bootstrap_method_attr_index, name_and_type_index *)

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
    | ITEM_Object of int (* cpool_index *)
    | ITEM_Uninitialized of int (* offset *)

  type stack_map_frame =
    | SAME of int (* offset_delta *)
    | SAME_LOCALS_1_STACK_ITEM of int * verification_type_info (* offset_delta, stack *)
    | SAME_LOCALS_1_STACK_ITEM_EXTENDED of int * verification_type_info (* offset_delta, stack *)
    | CHOP of int * int (* k, offset_delta *)
    | SAME_FRAME_EXTENDED of int (* offset_delta *)
    | APPEND of int * int * verification_type_info array (* k, offset_delta, locals *)
    | FULL_FRAME of int * verification_type_info array * verification_type_info array (* offset_delta, locals, stack *)

  type exception_table_info =
    {
      start_pc : int;
      end_pc : int;
      handler_pc : int;
      catch_type : int;
    }

  type code_attribute =
    {
      max_stack : int;
      max_locals : int;
      code : string;
      exception_table : exception_table_info array;
      attributes : attribute array;
    }

  and attribute_info =
    | ConstantValue of int (* constant_value_index *)
    | Code of code_attribute
    | StackMapTable of stack_map_frame array (* entries *)
    | Exceptions of int array (* exception_index_table *)
    | SourceFile of int (* sourcefile_index *)

  and attribute =
    {
      attribute_name_index : int;
      attribute_info : attribute_info;
    }

  let rec attribute_length = function
    | Code code ->
        8 + String.length code.code + 2 +
        8 * Array.length code.exception_table +
        2 + Array.fold_left (fun acc attr -> acc + 6 + attribute_length attr.attribute_info) 0 code.attributes
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
      magic : int32;
      minor_version : int;
      major_version : int;
      constant_pool : cp_info array;
      access_flags : access_flag list;
      this_class : int;
      super_class : int;
      interfaces : int array;
      fields : field_info array;
      methods : field_info array;
      attributes : attribute array;
    }

  let output_int32 oc n =
    output_binary_int oc (Int32.to_int n) (* FIXME *)

  let output_int16 oc n =
    output_byte oc ((n land 0xFF00) lsl 8);
    output_byte oc (n land 0xFF)

  let output_constant oc = function
    | CONSTANT_Class name_index ->
        output_byte oc 7;
        output_int16 oc name_index
    | CONSTANT_Utf8 bytes ->
        output_byte oc 1;
        output_int16 oc (String.length bytes);
        output_string oc bytes (* FIXME Unicode *)
    | CONSTANT_Integer n ->
        output_byte oc 3;
        output_int32 oc n
    | _ ->
        failwith "output_constant"

  let output_flags oc flags =
    let n = List.fold_left (fun acc flag -> acc lor (int_of_flag flag)) 0 flags in
    output_int16 oc n

  let output_field _oc = function
    | _ -> failwith "output_field"

  let output_exception_table_info _oc = function
    | _ -> failwith "output_exception_table_info"

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
end

module Compile = struct
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

  let compile_main stmt =
    let code = compile_stmt 0 M.empty stmt [] in
    let max_locals = !max_locals in
    let max_stack = !max_stack in
    {max_locals; max_stack; code}
end

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
      set_byte str idx 172

let get_code tbl code =
  let len = List.fold_left (fun acc i -> acc + Compile.instruction_size i) 0 code in
  let str = String.make len '\000' in
  let _ =
    List.fold_left (fun idx inst ->
        write_inst tbl str idx inst; idx + Compile.instruction_size inst
      ) 0 code
  in
  str

let test_main {Compile.max_locals; max_stack; code} =
  let tbl = Hashtbl.create 0 in
  let this_class = get_class tbl "Main" in
  let super_class = get_class tbl "java/lang/Object" in
  let code = get_code tbl code in
  let methods =
    [|
      {
        access_flags = [ACC_PUBLIC; ACC_STATIC; ACC_FINAL];
        name_index = get_utf8 tbl "main";
        descriptor_index = get_utf8 tbl "()I";
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
          |];
      }
    |]
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
    attributes = [| |];
  }

let () =
  let cls = test_main {Compile.max_stack = 0; max_locals = 0; code = []} in
  output_class stdout cls
