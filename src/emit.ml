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

let emit {Compile.max_locals; max_stack; code} =
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