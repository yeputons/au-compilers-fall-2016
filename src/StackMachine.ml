open GT
     @type i =
        | S_READ
        | S_WRITE
        | S_PUSH  of int
        | S_LD    of string
        | S_ST    of string
        | S_BINOP of string with show

let i_to_string = show(i)

module Interpreter =
struct
  open Language.Expr

  let run input code =
    let rec run' (state, stack, input, output) iptr =
      if iptr >= Array.length code then
        output
      else
        let i = code.(iptr) in
        run'
          (match i with
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

  let rec stmt = function
    | Skip          -> [||]
    | Assign (x, e) -> Array.append (expr e) [|S_ST x|]
    | Read    x     -> [|S_READ; S_ST x|]
    | Write   e     -> Array.append (expr e) [|S_WRITE|]
    | Seq    (l, r) -> Array.append (stmt l) (stmt r)
end
