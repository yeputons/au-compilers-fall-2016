open Expr
open Parser

(*
(*
read (x);
read (y);
z := x * x;
write (z+y)
*)
let p =
  Seq (
      Read "x",
      Seq (
          Read "y",
          Seq (
              Assign ("z", Mul (Var "x", Var "x")),
              Write (Add (Var "z", Var "y"))
          )
      )
    )

(* let _ = *)
(*   let [r] = run [3; 4] p in *)
(*   Printf.printf "%d\n" r *)

let ( !! )       = (!)
let ( !  ) x     = Var x
let ( $  ) n     = Const n
let ( +  ) e1 e2 = Add (e1, e2)
let ( *  ) e1 e2 = Mul (e1, e2)

let skip     = Skip
let (:=) x e = Assign (x, e)
let read x   = Read x
let write x  = Write x
let (|>) l r = Seq (l, r)

(*
read (x);
read (y);
z := x * x;
write (z+y)
*)

let p =
  read "x" |>
  read "y" |>
  ("z" := !"x" * !"x") |>
  write (!"z" + !"y")

(*
let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r

let run input p =
  srun input (compile_stmt p)

let _ =
  let [r] = run [3; 4] p in
  Printf.printf "%d\n" r
*)
*)

let main =
  let interpret_flag = ref false in
  let filename_arg : 'string option ref = ref None in
  let speclist =
    [("--interpret", Arg.Set interpret_flag, "Interpret program instead of compiling")] in
  let set_filename = (fun name ->
      match !filename_arg with
      | None -> filename_arg := Some name
      | (Some _) -> raise (Arg.Bad "More than one positional argument")
    ) in
  let usage_str = "Usage: rc.byte [--interpret] <name.expr>" in
  Arg.parse speclist set_filename usage_str;
  match !filename_arg with
  | None ->
      Printf.eprintf "No input file specified\n";
      exit 1;
  | (Some file) ->
      match Parser.parse file with
      | `Fail err -> Printf.eprintf "%s\n" err
      | `Ok stmt ->
        match !interpret_flag with
        | true ->
            Printf.eprintf "Interpreting %s...\n%!" file;
            let result = Expr.run [] stmt in
              List.iter (Printf.printf "%d\n") result
        | false ->
            ignore @@ Expr.build stmt (Filename.chop_suffix file ".expr")

