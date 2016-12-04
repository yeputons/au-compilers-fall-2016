open Language.Prog

let builtins : (string * int * (int list -> int)) list = [
  ("read", 0, fun [] ->
      Printf.printf "> ";
      read_int ()
  );
  ("write", 1, fun [v] ->
      Printf.printf "%d\n" v;
      0
  )
]

let builtins_fun = List.map (fun (n, a, _) -> (FunName n, Builtin a)) builtins
let builtins_impl = List.map (fun (n, _, f) -> (n, f)) builtins
