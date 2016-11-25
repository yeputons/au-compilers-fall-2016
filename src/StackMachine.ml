open GT
     @type fhead  = {args:string list; locals:string list; max_stack:int} with show
     @type i =
        | S_READ
        | S_WRITE
        | S_PUSH  of int
        | S_LD    of string
        | S_ST    of string
        | S_DROP
        | S_BINOP of string
        | S_LABEL of string
        | S_JMP   of string
        | S_JZ    of string
        | S_COMM  of string
        | S_CALL  of string * int
        | S_RET   of string
        | S_FUN_BEGIN of fhead
        | S_FUN_END
      with show

let i_to_string = show(i)

module S = Set.Make(String)

let used_vars code =
  let add_var i s = match i with
    | S_LD x | S_ST x -> S.add x s
    | _ -> s
  in
  S.elements @@ Array.fold_right add_var code S.empty

let max_stack code =
  let stack_change = function
    | S_READ    -> (0, true)
    | S_WRITE   -> (1, false)
    | S_PUSH  _ -> (0, true)
    | S_LD    _ -> (0, true)
    | S_ST    _ -> (1, false)
    | S_DROP    -> (1, false)
    | S_BINOP _ -> (2, true)
    | S_JZ    _ -> (1, false)
    | S_CALL (_, args) -> (args, true)
    | S_RET   _ -> (1, false)
    | S_LABEL _
    | S_JMP   _
    | S_COMM  _
    | S_FUN_BEGIN _
    | S_FUN_END -> (0, false)
  in
  let folder (c, m) i =
    let (args, ret) = stack_change i in
    let c' = c - args + (if ret then 1 else 0) in
    assert (c' >= 0);
    (c', max c' m)
  in
  let (0, m) = Array.fold_left folder (0, 0) code in
  m

module Interpreter =
struct
  open Language.Expr

  let run reader writer code =
    let labels =
      List.concat
        (List.mapi
           (function i -> function (S_LABEL x) -> [(x, i)] | _ -> [])
           (Array.to_list code))
    in
    let rec run' (state, stack) iptr allowed_vars =
      if iptr >= Array.length code then
        failwith "Should not reach end of the code"
      else
        match code.(iptr) with
        | S_JMP lbl -> run' (state, stack) (List.assoc lbl labels) allowed_vars
        | S_RET lbl ->
          let iptr' = List.assoc lbl labels in
          (match code.(iptr' + 1) with
            | S_FUN_END -> ()
            | _         -> failwith "Non-S_FUN_END after S_RET's label"
          );
          let [y] = stack in
          (state, stack)
        | S_JZ lbl ->
          let x::stack' = stack in
          let iptr' =
            if (x = 0)
            then (List.assoc lbl labels)
            else (iptr + 1) in
          run' (state, stack') iptr' allowed_vars
        | S_FUN_BEGIN {args; locals; _} ->
          assert (allowed_vars = []);
          assert (List.length args = List.length stack);
          run' (List.combine args stack, []) (iptr + 1) (List.append args locals)
        | S_FUN_END ->
          failwith "Should not reach S_FUN_END"
        | _ ->
        run'
          (match code.(iptr) with
           | S_READ ->
             let y = reader () in
             (state, y::stack)
           | S_WRITE ->
             let y::stack' = stack in
             writer y;
             (state, stack')
           | S_PUSH n ->
             (state, n::stack)
           | S_LD x ->
             assert (List.mem x allowed_vars);
             (state, (List.assoc x state)::stack)
           | S_ST x ->
             assert (List.mem x allowed_vars);
             let y::stack' = stack in
             ((x, y)::state, stack')
           | S_DROP ->
             let _::stack' = stack in
             (state, stack')
           | S_BINOP s ->
             let r::l::stack' = stack in
             (state, (eval_binop s l r)::stack')
           | S_LABEL s ->
             (state, stack)
           | S_COMM _ ->
             (state, stack)
           | S_CALL (lbl, args_cnt) ->
             let iptr' = List.assoc lbl labels in
             let rec split n l =
               if n > 0 then
                 let x::xs = l in
                 let (a, b) = split (n - 1) xs in
                 (x::a, b)
               else
                 ([], l)
             in
             let (args, stack') = split args_cnt stack in
             let (_, [res]) = run' ([], args) iptr' [] in
             (state, res::stack')
          )
          (iptr + 1)
          allowed_vars
    in
    let (_, [ret]) = run' ([], []) (List.assoc "main" labels) [] in
    ()
end

class smenv =
  object(self)
    val mutable last_lbl_id = 0
    method next_lbl =
      last_lbl_id <- last_lbl_id + 1;
      Printf.sprintf "lbl_%d" last_lbl_id

    val mutable end_lbl = ""
    method next_end_lbl = end_lbl <- self#next_lbl; end_lbl
    method cur_end_lbl = end_lbl
  end

module Compile =
struct

  open Language.Expr
  open Language.Stmt
  open Language.Prog

  let rec expr = function
    | Var   x -> [|S_LD   x|]
    | Const n -> [|S_PUSH n|]
    | Binop (s, x, y) -> Array.concat [expr x; expr y; [|S_BINOP s|]]
    | FunCall (name, args) -> Array.concat [
        Array.concat @@ List.map expr args;
        [|S_CALL ("fun_" ^ name, List.length args)|];
      ]

  let stmt env =
    let rec stmt' = function
    | Skip          -> [|S_COMM "skip"|]
    | Assign (x, e) -> Array.concat [
        [|S_COMM (x ^ " := " ^ (t_to_string e))|];
        expr e;
        [|S_ST x|]
      ]
    | Read    x     -> [|
        S_COMM ("read(" ^ x ^ ")");
        S_READ;
        S_ST x
      |]
    | Write   e     -> Array.concat [
        [|S_COMM ("write(" ^ (t_to_string e) ^ ")")|];
        expr e;
        [|S_WRITE|];
      ]
    | Seq    (l, r) -> Array.append (stmt' l) (stmt' r)
    | If     (c, t, f) ->
      let lbl_else = env#next_lbl in
      let lbl_end = env#next_lbl in
      Array.concat [
        [|S_COMM ("if " ^ (t_to_string c))|];
        expr c; [|S_JZ lbl_else|];
        [|S_COMM "then {"|];
        stmt' t; [|S_JMP lbl_end|];
        [|S_COMM "} else {"|];
        [|S_LABEL lbl_else|];
        stmt' f;
        [|S_COMM "}"|];
        [|S_LABEL lbl_end|];
      ]
    | While  (c, s) ->
      let lbl_begin = env#next_lbl in
      let lbl_end = env#next_lbl in
      Array.concat [
        [|S_COMM ("while " ^ (t_to_string c))|];
        [|S_LABEL lbl_begin|];
        expr c; [|S_JZ lbl_end|];
        [|S_COMM "do {"|];
        stmt' s; [|S_JMP lbl_begin|];
        [|S_COMM "}"|];
        [|S_LABEL lbl_end|];
      ]
    | Until (s, c) ->
      let lbl_begin = env#next_lbl in
      Array.concat [
        [|S_COMM "repeat {"|];
        [|S_LABEL lbl_begin|];
        stmt' s;
        [|S_COMM ("} until " ^ (t_to_string c))|];
        expr c; [|S_JZ lbl_begin|];
      ]
    | Ignore (e) -> expr e
    | Return (e) ->
      Array.concat [
        [|S_COMM ("return " ^ (t_to_string e))|];
        expr e;
        [|S_RET env#cur_end_lbl|];
      ]
    in
    stmt'

  let prog (p:t) : i array =
    let env = new smenv in
    let comp_fun : f -> i array  = function
      | Fun (args, body) ->
        let lbl_end = env#next_end_lbl in
        let body = stmt env body in
        let locals = S.elements @@ S.diff (S.of_list (used_vars body)) (S.of_list args) in
        let max_stack = max_stack body in
        Array.concat [
          [|S_FUN_BEGIN {args=args; locals=locals; max_stack=max_stack}|];
          body;
          [|S_COMM "FUN_END";
            S_PUSH 0xDEADBEEF;
            S_RET lbl_end;
            S_LABEL lbl_end;
            S_FUN_END|];
        ]
    in
    let conv_fun (name, body) : i array =
      let body = comp_fun body in
      Array.append (match name with
        | FunName name -> [|S_COMM ("FUN " ^ name); S_LABEL ("fun_" ^ name)|]
        | ProgBody -> [|S_COMM ("MAIN BODY"); S_LABEL "main"|]
      ) body
    in
    Array.concat @@ List.map conv_fun p
end
