open Util

module Expr =
struct

  open Language.Expr

  let rec eval funs var_get = function
    | Const  n -> n
    | Var    x -> var_get x
    | Binop  (op, l, r) ->
      let lv = eval funs var_get l in
      let rv = eval funs var_get r in
      eval_binop op lv rv
    | FunCall (fname, args) ->
      (assoc_err fname funs "Function '%s' not found") (List.map (eval funs var_get) args)

end

module Stmt =
struct

  open Language.Prog
  open Language.Stmt

  type t_state =
    | Computing of (string * int) list
    | Returned  of int

  let eval funs stmt =
    let rec eval' (state:t_state) (stmt:t) : t_state =
      let funs = List.map (fun (name, fun_eval) -> (name, fun_eval eval')) funs in
      match state with
      | Returned x -> Returned x
      | Computing vars ->
        let var_get x = List.assoc x vars in
        let expr_eval = Expr.eval funs var_get in
        match stmt with
        | Skip          -> state
        | Seq    (l, r) -> eval' (eval' state l) r
        | Assign (x, e) -> Computing ((x, expr_eval e)::vars)
        | Ignore  e     -> ignore @@ expr_eval e; Computing vars
        | Return  e     -> Returned (expr_eval e)
        | If (e, s1, s2) ->
          let v = expr_eval e in
          eval' state (if v <> 0 then s1 else s2)
        | While (e, s) ->
          let v = expr_eval e in
          if v <> 0 then
            eval' (eval' state s) stmt
          else
            state
        | Until (s, e) ->
          let state = eval' state s in
          match state with
          | Returned x -> Returned x
          | Computing vars ->
            let var_get x = List.assoc x vars in
            let expr_eval = Expr.eval funs var_get in
            let v = expr_eval e in
            if v = 0 then
              eval' state stmt
            else
              state
    in
    (* We do not want returns inside main body *)
    let (Computing _) = eval' (Computing []) stmt in
    ()

end

module Prog =
struct
  open Stmt
  open Language.Prog

  let eval prog =
    let Fun ([], body) = List.assoc ProgBody prog in
    let fun_transform = function
      | (FunName name, impl) ->
        let fun_eval stmt_eval arg_vals =
          let args_cnt = match impl with
            | Fun (arg_names, body) -> List.length arg_names
            | Builtin x -> x
          in
          if (args_cnt != List.length arg_vals) then
            failwith @@ Printf.sprintf "Invalid number of arguments for function '%s': expected %d, found %d"
              name args_cnt (List.length arg_vals)
          else
            match impl with
            | Fun (arg_names, body) ->
              let vars = (List.combine arg_names arg_vals) in
              let (Returned res) = stmt_eval (Computing vars) body in
              res
            | Builtin _ ->
              (List.assoc name Runtime.builtins_impl) arg_vals
        in
        [(name, fun_eval)]
      | (ProgBody, _) -> []
    in
    let funs = List.flatten @@ List.map fun_transform (prog @ Runtime.builtins) in
    Stmt.eval funs body
end
