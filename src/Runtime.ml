open Language.Prog

let read [] =
  Printf.printf "> ";
  read_int ()

let write [x] =
  Printf.printf "%d\n" x;
  0

let builtins = [
  (FunName "read", Builtin 0);
  (FunName "write", Builtin 1)
]
let builtins_impl = [
  ("read", read);
  ("write", write)
]
