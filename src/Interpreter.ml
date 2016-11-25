module Expr =
struct

  open Language.Expr

  let rec eval var_get = function
    | Const  n -> n
    | Var    x -> var_get x
    | Binop  (op, l, r) ->
      let lv = eval var_get l in
      let rv = eval var_get r in
      eval_binop op lv rv

end

module Stmt =
struct

  open Language.Stmt

  let eval reader writer stmt =
    let rec eval' state stmt =
      let var_get x = List.assoc x state in
      match stmt with
      | Skip          -> state
      | Seq    (l, r) -> eval' (eval' state l) r
      | Assign (x, e) -> (x, Expr.eval var_get e) :: state
      | Write   e     ->
        writer (Expr.eval var_get e);
        state
      | Read    x     ->
        let y = reader () in
        (x, y) :: state
      | If (e, s1, s2) ->
        let v = Expr.eval var_get e in
        eval' state (if v <> 0 then s1 else s2)
      | While (e, s) ->
        let v = Expr.eval var_get e in
        if v <> 0 then
          eval' (eval' state s) stmt
        else
          state
      | Until (s, e) ->
        let state = eval' state s in
        let var_get x = List.assoc x state in
        let v = Expr.eval var_get e in
        if v = 0 then
          eval' state stmt
        else
          state
    in
    eval' [] stmt;
    ()

end
