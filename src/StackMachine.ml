open GT
     @type i =
        | S_READ
        | S_WRITE
        | S_PUSH  of int
        | S_LD    of string
        | S_ST    of string
        | S_BINOP of string
        | S_LABEL of string
        | S_JMP   of string
        | S_JZ    of string
        | S_COMM  of string
      with show

let i_to_string = show(i)

module S = Set.Make(String)

let used_vars code =
  let add_var i s = match i with
    | S_LD x | S_ST x -> S.add x s
    | _ -> s
  in
  S.elements @@ Array.fold_right add_var code S.empty

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
    let rec run' (state, stack) iptr =
      if iptr >= Array.length code then
        (state, stack)
      else
        match code.(iptr) with
        | S_JMP lbl -> run' (state, stack) (List.assoc lbl labels)
        | S_JZ lbl ->
          let x::stack' = stack in
          let iptr' =
            if (x = 0)
            then (List.assoc lbl labels)
            else (iptr + 1) in
          run' (state, stack') iptr'
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
             (state, (List.assoc x state)::stack)
           | S_ST x ->
             let y::stack' = stack in
             ((x, y)::state, stack')
           | S_BINOP s ->
             let r::l::stack' = stack in
             (state, (eval_binop s l r)::stack')
           | S_LABEL s ->
             (state, stack)
           | S_COMM _ ->
             (state, stack)
          )
          (iptr + 1)
    in
    run' ([], []) 0;
    ()
end

class smenv =
  object(self)
    val mutable last_lbl_id = 0
    method next_lbl =
      last_lbl_id <- last_lbl_id + 1;
      Printf.sprintf "lbl_%d" last_lbl_id
  end

module Compile =
struct

  open Language.Expr
  open Language.Stmt

  let rec expr = function
    | Var   x -> [|S_LD   x|]
    | Const n -> [|S_PUSH n|]
    | Binop (s, x, y) -> Array.concat [expr x; expr y; [|S_BINOP s|]]

  let stmt =
    let env = new smenv in
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
    in
    stmt'
end
