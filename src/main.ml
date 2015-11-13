let () =
  let cls = Emit.emit {Compile.max_stack = 0; max_locals = 0; code = []} in
  ClassFile.output_class stdout cls
