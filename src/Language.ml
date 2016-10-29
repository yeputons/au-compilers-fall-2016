open Ostap
open Matcher

module Expr =
struct

  type t =
    | Const of int
    | Var   of string
    | Binop of string * t * t

  let eval_binop s x y =
    match s with
    | "+" -> x + y
    | "-" -> x - y
    | "*" -> x * y
    | "/" -> x / y
    | "%" -> x mod y
    | "<=" -> if x <= y then 1 else 0
    | ">=" -> if x >= y then 1 else 0
    | "<"  -> if x <  y then 1 else 0
    | ">"  -> if x >  y then 1 else 0
    | "==" -> if x =  y then 1 else 0
    | "!=" -> if x <> y then 1 else 0
    | "&&" -> if (x <> 0) && (y <> 0) then 1 else 0
    | "!!" -> if (x <> 0) || (y <> 0) then 1 else 0

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

  ostap (
    parse: s:simple d:(-";" parse)? {
        match d with None -> s | Some d -> Seq (s, d)
      };

    simple:
      x:IDENT ":=" e:!(Expr.parse)     {Assign (x, e)}
    | %"read"  "(" x:IDENT ")"         {Read x}
    | %"write" "(" e:!(Expr.parse) ")" {Write e}
    | %"skip"                          {Skip}
  )

end
