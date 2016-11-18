open Ostap
open Matcher

module Expr =
struct

  type t =
    | Const of int
    | Var   of string
    | Binop of string * t * t

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
    parse: ori;

    ori:
      l:andi suf:("!!" andi)* {
            List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
      }
    | andi;

    andi:
      l:cmpi suf:("&&" cmpi)* {
        List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
      }
    | cmpi;

    cmpi:
      l:addi suf:(("<=" | "<" | "==" | "!=" | ">=" | ">") addi)* {
        List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
      }
    | addi;

    addi:
      l:mulli suf:(("+" | "-") mulli)* {
        List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
      }
    | mulli;

    mulli:
      l:primary suf:(("*" | "/" | "%") primary)* {
        List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
      }
    | primary;

    primary:
      n:DECIMAL {Const n}
    | x:IDENT   {Var   x}
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

  ostap (
    parse: s:simple d:(-";" parse)? {
        match d with None -> s | Some d -> Seq (s, d)
      };

    simple:
      x:IDENT ":=" e:!(Expr.parse)     {Assign (x, e)}
    | %"read"  "(" x:IDENT ")"         {Read x}
    | %"write" "(" e:!(Expr.parse) ")" {Write e}
    | %"skip"                          {Skip}
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
       Seq (s, While (Binop ("==", e, Const 0), s))
      }
    | %"for" s1:parse "," e:!(Expr.parse) "," s2:parse "do" s:parse "od" {
       Seq (s1, While (e, Seq (s, s2)))
      }
  )

end
