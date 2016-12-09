open Ostap
open Matcher

module Value =
struct
  type t =
    | Int   of int
    | Str   of bytes
    | Arr   of bool * t array

  let rec t_to_string = function
    | Int c -> Printf.sprintf "%d" c
    | Str c -> Printf.sprintf "\"%s\"" c
    | Arr (boxed, a) ->
      let vals = List.map t_to_string (Array.to_list a) in
      Printf.sprintf (if boxed then "{%s}" else "[%s]") @@ String.concat "," vals
end

module Expr =
struct
  open Value

  type t =
    | Const   of Value.t
    | Var     of string
    | Binop   of string * t * t
    | FunCall of string * t list
    | Elem    of t * t
    | Arr     of bool * t list

  let bti b = if b then 1 else 0
  let itb i = i <> 0

  let rec t_to_string = function
    | Const c -> Value.t_to_string c
    | Var name -> name
    | Binop (op, a, b) -> Printf.sprintf "(%s %s %s)" (t_to_string a) op (t_to_string b)
    | FunCall (name, args) ->
      let args_str = List.map t_to_string args in
      Printf.sprintf "%s(%s)" name (String.concat ", " args_str)
    | Elem (arr, el) ->
      Printf.sprintf "%s[%s]" (t_to_string arr) (t_to_string el)

  let eval_binop s (Int x) (Int y) = Int (
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
            unops);

    unops:
      p:primary idx:(-"[" !(parse) -"]")* {
        List.fold_left (fun e i -> Elem (e, i)) p idx
      };

    primary:
      "true" {Const (Int 1)}
    | "false" {Const (Int 0)}
    | n:DECIMAL {Const (Int n)}
    | s:STRING {Const (Str (Bytes.sub s 1 (Bytes.length s - 2)))}
    | c:CHAR {Const (Int (Char.code c))}
    | x:IDENT call:(-"(" !(Util.list0 parse) -")")? {
      match call with
      | Some (args) -> FunCall(x, args)
      | None -> Var x
    }
    | -"[" v:!(Util.list0 parse) -"]" { Arr (false, v) }
    | -"{" v:!(Util.list0 parse) -"}" { Arr (true, v) }
    | -"(" parse -")"
  )

end

module Stmt =
struct

  type t =
    | Skip
    | Assign of string * Expr.t
    | AssignArr of string * Expr.t list * Expr.t
    | Seq    of t * t
    | If     of Expr.t * t * t
    | While  of Expr.t * t
    | Until  of t * Expr.t
    | Ignore of Expr.t
    | Return of Expr.t

  let keywords = ["skip"; "return"; "if"; "then"; "elif"; "else"; "fi"; "while"; "do"; "od"; "repeat"; "until"; "for"]

  ostap (
    parse: s:simple d:(-";" parse)? {
        match d with None -> s | Some d -> Seq (s, d)
      };

    simple:
      x:IDENT res:(
                idx:(-"[" !(Expr.parse) -"]")*
                ":=" e:!(Expr.parse) {
                  match idx with
                  | [] -> Assign (x, e)
                  | _  -> AssignArr (x, idx, e)
                }
              | "(" args:!(Util.list0 Expr.parse) ")" {
                    Ignore (FunCall (x, args))
                }
              )
      { res }
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
  type f = Fun of string list * Stmt.t | Builtin of int
  type t = (fname * f) list

  let keywords = Stmt.keywords @ ["fun"; "begin"; "end"]

  ostap (
    parse: fs:funcdef* s:!(Stmt.parse) {
        (ProgBody, Fun ([], s))::fs
      };

    funcdef: %"fun" n:IDENT "(" args:!(Util.list0 arg) ")" "begin" s:!(Stmt.parse) "end" {
        (FunName n, Fun (args, s))
      };

    arg: IDENT
  )

  let parse_str s =
    Util.parse
      (object
        inherit Matcher.t s
        inherit Util.Lexers.ident keywords s
        inherit Util.Lexers.decimal s
        inherit Util.Lexers.string s
        inherit Util.Lexers.char s
        inherit Util.Lexers.skip [
            Matcher.Skip.whitespaces " \t\n";
            Matcher.Skip.lineComment "--";
            Matcher.Skip. nestedComment "(*" "*)"
          ] s
      end
      )
      (ostap (!(parse) -EOF))

end
