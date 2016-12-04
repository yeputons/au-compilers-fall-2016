open Language.Value
open Language.Prog

let builtins : (string * int * (Language.Value.t list -> Language.Value.t)) list = [
  ("read", 0, fun [] ->
      Printf.printf "> ";
      Int (read_int ())
  );
  ("write", 1, fun [Int v] ->
      Printf.printf "%d\n" v;
      Int 0
  );
  ("writes", 1, fun [Str v] ->
      Printf.printf "%s\n" v;
      Int 0
  );
]

let builtins_fun = List.map (fun (n, a, _) -> (FunName n, Builtin a)) builtins
let builtins_impl = List.map (fun (n, _, f) -> (n, f)) builtins
