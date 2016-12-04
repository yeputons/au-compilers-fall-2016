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
  ("strmake", 2, fun [Int n; Int c] -> Str (Bytes.make n (Char.chr c)));
  ("strset", 3, fun [Str s; Int i; Int c] -> Bytes.set s i (Char.chr c); Str s);
  ("strget", 2, fun [Str s; Int i] -> Int (Char.code @@ Bytes.get s i));
  ("strdup", 1, fun [Str s] -> Str (Bytes.copy s));
  ("strcat", 2, fun [Str s1; Str s2] -> Str (Bytes.concat "" [s1; s2]));
  ("strcmp", 2, fun [Str s1; Str s2] ->
                    let res = Bytes.compare s1 s2 in
                    match res with
                    | 0 -> Int 0
                    | _ -> Int (if res > 0 then 1 else -1)
  );
  ("strlen", 1, fun [Str s] -> Int (Bytes.length s));
  ("strsub", 3, fun [Str s; Int i; Int l] -> Str (Bytes.sub s i l));
]

let builtins_fun = List.map (fun (n, a, _) -> (FunName n, Builtin a)) builtins
let builtins_impl = List.map (fun (n, _, f) -> (n, f)) builtins
