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
      with show

let i_to_string = show(i)

module Interpreter =
struct
  open Language.Expr

  let run input code =
    let labels =
      List.concat
        (List.mapi
           (function i -> function (S_LABEL x) -> [(x, i)] | _ -> [])
           (Array.to_list code))
    in
    let rec run' (state, stack, input, output) iptr =
      if iptr >= Array.length code then
        output
      else
        match code.(iptr) with
        | S_JMP lbl -> run' (state, stack, input, output) (List.assoc lbl labels)
        | S_JZ lbl ->
          let x::stack' = stack in
          let iptr' =
            if (x = 0)
            then (List.assoc lbl labels)
            else (iptr + 1) in
          run' (state, stack', input, output) iptr'
        | _ ->
        run'
          (match code.(iptr) with
           | S_READ ->
             let y::input' = input in
             (state, y::stack, input', output)
           | S_WRITE ->
             let y::stack' = stack in
             (state, stack', input, output @ [y])
           | S_PUSH n ->
             (state, n::stack, input, output)
           | S_LD x ->
             (state, (List.assoc x state)::stack, input, output)
           | S_ST x ->
             let y::stack' = stack in
             ((x, y)::state, stack', input, output)
           | S_BINOP s ->
             let r::l::stack' = stack in
             (state, (eval_binop s l r)::stack', input, output)
           | S_LABEL s ->
             (state, stack, input, output)
          )
          (iptr + 1)
    in
    run' ([], [], input, []) 0
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
    let last_lbl_id = ref 0 in
    let next_lbl () =
      last_lbl_id := !last_lbl_id + 1;
      Printf.sprintf "lbl_%d" !last_lbl_id
    in
    let rec stmt' = function
    | Skip          -> [||]
    | Assign (x, e) -> Array.append (expr e) [|S_ST x|]
    | Read    x     -> [|S_READ; S_ST x|]
    | Write   e     -> Array.append (expr e) [|S_WRITE|]
    | Seq    (l, r) -> Array.append (stmt' l) (stmt' r)
    | If     (c, t, f) ->
      let lbl_else = next_lbl () in
      let lbl_end = next_lbl () in
      Array.concat [
        expr c; [|S_JZ lbl_else|];
        stmt' t; [|S_JMP lbl_end|];
        [|S_LABEL lbl_else|];
        stmt' f;
        [|S_LABEL lbl_end|]
      ]
    | While  (c, s) ->
      let lbl_begin = next_lbl() in
      let lbl_end = next_lbl() in
      Array.concat [
        [|S_LABEL lbl_begin|];
        expr c; [|S_JZ lbl_end|];
        stmt' s; [|S_JMP lbl_begin|];
        [|S_LABEL lbl_end|];
      ]
    | Until (s, c) ->
      let lbl_begin = next_lbl() in
      Array.concat [
        [|S_LABEL lbl_begin|];
        stmt' s;
        expr c; [|S_JZ lbl_begin|];
      ]
    in
    stmt'
end
