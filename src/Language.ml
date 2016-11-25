open Ostap
open Matcher

module Expr =
struct

  type t =
    | Const   of int
    | Var     of string
    | Binop   of string * t * t
    | FunCall of string * t list

  let bti b = if b then 1 else 0
  let itb i = i <> 0

  let eval_binop s x y =
    match s with
    | "+" -> x + y
    | "-" -> x - y
    | "*" -> x * y
    | "/" -> x / y
    | "%" -> x mod y
    | _ -> bti (match s with
      | "<=" -> x <= y
      | ">=" -> x >= y
      | "<"  -> x <  y
      | ">"  -> x >  y
      | "==" -> x =  y
      | "!=" -> x <> y
      | "&&" -> itb x && itb y
      | "!!" -> itb x || itb y
      )

  ostap (
    parse:
        !(Ostap.Util.expr
            (fun x -> x)
            (Array.map (fun (a, s) -> a,
                            List.map  (fun s -> ostap(- $(s)),
                                       (fun x y -> Binop (s, x, y))
                                      ) s
                       )
                       [|
                            `Lefta, ["!!"];
                            `Lefta, ["&&"];
                            `Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
                            `Lefta, ["+" ; "-"];
                            `Lefta, ["*" ; "/"; "%"];
                       |]
            )
            primary);

    primary:
      n:DECIMAL {Const n}
    | x:IDENT call:(-"(" !(Util.list0 parse) -")")? {
      match call with
      | Some (args) -> FunCall(x, args)
      | None -> Var x
    }
    | -"(" parse -")"
  )

end

module Stmt =
struct

  type t =
    | Skip
    | Read   of string
    | Write  of Expr.t
    | Assign of string * Expr.t
    | Seq    of t * t
    | If     of Expr.t * t * t
    | While  of Expr.t * t
    | Until  of t * Expr.t
    | Ignore of Expr.t
    | Return of Expr.t

  ostap (
    parse: s:simple d:(-";" parse)? {
        match d with None -> s | Some d -> Seq (s, d)
      };

    simple:
      x:IDENT res:(
                ":=" e:!(Expr.parse) {Assign (x, e)}
              | "(" args:!(Util.list0 Expr.parse) ")" {
                    Ignore (FunCall (x, args))
                }
              )
      { res }
    | %"read"  "(" x:IDENT ")"         {Read x}
    | %"write" "(" e:!(Expr.parse) ")" {Write e}
    | %"skip"                          {Skip}
    | %"return" e:!(Expr.parse)        {Return e}
    | %"if" e1:!(Expr.parse) "then" s1:parse
          ss:(-"elif" !(Expr.parse) -"then" parse)*
             seopt:(-"else" parse)? "fi" {
        let se = match seopt with None -> Skip | Some x -> x in
        let conds = (e1, s1)::ss in
        List.fold_right (fun (e, s) r -> If (e, s, r)) conds se
      }
    | %"while" e:!(Expr.parse) "do" s:parse "od" {
        While (e, s)
      }
    | %"repeat" s:parse "until" e:!(Expr.parse) {
        Until (s, e)
      }
    | %"for" s1:parse "," e:!(Expr.parse) "," s2:parse "do" s:parse "od" {
       Seq (s1, While (e, Seq (s, s2)))
      }
  )

end

module Prog =
struct
  type fname = FunName of string | ProgBody
  type f = Fun of string list * Stmt.t
  type t = (fname * f) list

  ostap (
    parse: fs:funcdef* s:!(Stmt.parse) {
        (ProgBody, Fun ([], s))::fs
      };

    funcdef: %"fun" n:IDENT "(" args:!(Util.list0 arg) ")" "begin" s:!(Stmt.parse) "end" {
        (FunName n, Fun (args, s))
      };

    arg: IDENT
  )

end
