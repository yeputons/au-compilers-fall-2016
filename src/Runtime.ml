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
  ("writeb", 1, fun [v] ->
      let rec print_val boxed = function
        | Arr (boxed', v) ->
          assert boxed;
          print_arr boxed' v
        | Str v ->
          assert boxed;
          v
        | Int v ->
          assert (not boxed);
          Printf.sprintf "%d" v
      and print_arr boxed (a:tarr) =
        let vals = String.concat ", " @@ match a with
          | LastDim vs -> List.map (print_val boxed) @@ Array.to_list vs
          | MidDim vs  -> List.map (print_arr boxed) @@ Array.to_list vs
        in
        Printf.sprintf (if boxed then "{%s}" else "[%s]") vals
      in
      let write' = function
        | Str v -> Printf.sprintf "%s" v;
        | Arr (boxed, a) -> print_arr boxed a
      in
      Printf.printf "%s\n" (write' v);
      Int 0
  );
  ("strmake", 2, fun [Int n; Int c] -> Str (Bytes.make n (Char.chr c)));
  ("strset", 3, fun [Str s; Int i; Int c] -> Bytes.set s i (Char.chr c); Str s);
  ("strget", 2, fun [Str s; Int i] -> Int (Char.code @@ Bytes.get s i));
  ("strdup", 1, fun [Str s] -> Str (Bytes.copy s));
  ("strcat", 2, fun [Str s1; Str s2] -> Str (Bytes.concat "" [s1; s2]));
  ("strcmp", 2, fun [Str s1; Str s2] ->
                    (* TODO: see X86.ml: what type of bytes does Bytes.compare compare? *)
                    let res = Bytes.compare s1 s2 in
                    match res with
                    | 0 -> Int 0
                    | _ -> Int (if res > 0 then 1 else -1)
  );
  ("strlen", 1, fun [Str s] -> Int (Bytes.length s));
  ("strsub", 3, fun [Str s; Int i; Int l] -> Str (Bytes.sub s i l));
  ("arrlen", 1, fun [Arr (_, LastDim a)] -> Int (Array.length a));
  ("arrmake", 2, fun [Int n; v] -> Arr (false, LastDim (Array.make n v)));
  ("Arrmake", 2, fun [Int n; v] -> Arr (true, LastDim (Array.make n v)));
  ("arrlenm", 2, fun [Arr (_, a); Int i] ->
      let rec get_len a = function
        | 0 -> (match a with LastDim a -> Int (Array.length a)
                           | MidDim a -> Int (Array.length a))
        | i -> (match a with MidDim a -> get_len (Array.get a 0) (i - 1))
      in
      get_len a i
  )
] @ (
    let rec make_arr v = function
      | [Int n] -> LastDim (Array.make n v)
      | (Int n)::dims' -> MidDim (Array.init n (fun _ -> make_arr v dims'))
    in
    [
      ("arrmakem", -1, fun ((Int d)::v::dims) ->
          assert (List.length dims == d);
          Arr (false, make_arr v dims)
      );
      ("Arrmakem", -1, fun ((Int d)::v::dims) ->
          assert (List.length dims == d);
          Arr (true, make_arr v dims)
      )
    ]
  )

let builtins_fun = List.map (fun (n, a, _) -> (FunName n, Builtin a)) builtins
let builtins_impl = List.map (fun (n, _, f) -> ("bi_" ^ n, f)) builtins
