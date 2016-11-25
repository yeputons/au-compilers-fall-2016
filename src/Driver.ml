open Ostap

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
      inherit Matcher.t s
      inherit Util.Lexers.ident ["read"; "write"; "skip"; "if"; "then"; "elif"; "fi"; "while"; "do"; "od"; "repeat"; "until"; "for"] s
      inherit Util.Lexers.decimal s
      inherit Util.Lexers.skip [
          Matcher.Skip.whitespaces " \t\n";
          Matcher.Skip.lineComment "--";
          Matcher.Skip. nestedComment "(*" "*)"
        ] s
    end
    )
    (ostap (!(Language.Prog.parse) -EOF))

let main = ()
    try
      let mode, filename =
        match Sys.argv.(1) with
        | "-s" -> `SM , Sys.argv.(2)
        | "-o" -> `X86, Sys.argv.(2)
        | "-i" -> `Int, Sys.argv.(2)
        | _ -> raise (Invalid_argument "invalid flag")
      in
      match parse filename with
      | `Ok prog ->
        let Language.Prog.Fun ([], stmt) = List.assoc Language.Prog.ProgBody prog in
        (match mode with
         | `X86 ->
           let basename = Filename.chop_suffix filename ".expr" in 
           X86.build stmt basename
         | _ ->
           let reader () =
             Printf.printf "> ";
             read_int ()
           in
           let writer x =
             Printf.printf "%d\n" x
           in
           match mode with
           | `SM -> StackMachine.Interpreter.run reader writer (StackMachine.Compile.stmt stmt)
           | `Int   -> Interpreter.Prog.eval reader writer prog
        )

      | `Fail er -> Printf.eprintf "%s\n" er
    with 
    | Invalid_argument _ ->
      Printf.printf "Usage: rc.byte <command> <name.expr>\n";
      Printf.printf "  <command> should be one of: -i, -s, -o\n"
