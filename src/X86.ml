type opnd = R of int | S of int | M of string | L of int

let x86regs = [|
  "%eax"; 
  "%edx";
  "%ebx"; 
  "%ecx"; 
  "%esi"; 
  "%edi"
|]

let num_of_regs = Array.length x86regs
let first_free_reg = 2
let word_size = 4

let eax = R 0
let edx = R 1
let ebx = R 2
let ecx = R 3
let esi = R 4
let edi = R 5

type instr =
| X86Add  of opnd * opnd
| X86Sub  of opnd * opnd
| X86Mul  of opnd * opnd
| X86Div  of opnd
| X86And  of opnd * opnd
| X86Or   of opnd * opnd
| X86Mov  of opnd * opnd
| X86Cdq
| X86Cmp  of opnd * opnd
| X86SeteAl
| X86SetneAl
| X86SetneDl
| X86SetlAl
| X86SetgAl
| X86SetleAl
| X86SetgeAl
| X86Push of opnd
| X86Pop  of opnd
| X86Ret
| X86Call of string

module S = Set.Make (String)

class x86env =
  object(self)
    val    local_vars = ref S.empty
    method local x    = local_vars := S.add x !local_vars
    method local_vars = S.elements !local_vars

    val    allocated  = ref 0
    method allocate n = allocated := max n !allocated
    method allocated  = !allocated
  end

let allocate env stack =
  match stack with
  | []                              -> R first_free_reg
  | (S n)::_                        -> env#allocate (n+1); S (n+1)
  | (R n)::_ when n < num_of_regs-1 -> R (n+1)
  | _                               -> S 0

module Show =
  struct

    let opnd = function
    | R i -> x86regs.(i)
    | S i -> Printf.sprintf "-%d(%%ebp)" (i * word_size)
    | M x -> x
    | L i -> Printf.sprintf "$%d" i

    let instr = function
    | X86Add (s1, s2) -> Printf.sprintf "\taddl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Sub (s1, s2) -> Printf.sprintf "\tsubl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Mul (s1, s2) -> Printf.sprintf "\timull\t%s,\t%s" (opnd s1) (opnd s2)
    | X86Div s2       -> Printf.sprintf "\tidivl\t%s"      (opnd s2)
    | X86And (s1, s2) -> Printf.sprintf "\tandl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Or  (s1, s2) -> Printf.sprintf "\torl\t%s,\t%s"   (opnd s1) (opnd s2)
    | X86Mov (s1, s2) -> Printf.sprintf "\tmovl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Cdq          -> Printf.sprintf "\tcdq"
    | X86Cmp (s1, s2) -> Printf.sprintf "\tcmpl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86SeteAl       -> Printf.sprintf "\tsete\t%%al"
    | X86SetneAl      -> Printf.sprintf "\tsetne\t%%al"
    | X86SetneDl      -> Printf.sprintf "\tsetne\t%%dl"
    | X86SetlAl       -> Printf.sprintf "\tsetl\t%%al"
    | X86SetgAl       -> Printf.sprintf "\tsetg\t%%al"
    | X86SetleAl      -> Printf.sprintf "\tsetle\t%%al"
    | X86SetgeAl      -> Printf.sprintf "\tsetge\t%%al"
    | X86Push s       -> Printf.sprintf "\tpushl\t%s"      (opnd s )
    | X86Pop  s       -> Printf.sprintf "\tpopl\t%s"       (opnd s )
    | X86Ret          -> "\tret"
    | X86Call p       -> Printf.sprintf "\tcall\t%s" p

  end

module Compile =
  struct

    open StackMachine

    let stack_program env code =
      let rec compile stack code =
        match code with
        | []       -> []
        | i::code' ->
            let (stack', x86code) =
              match i with
              | S_READ   ->
                  let s = allocate env stack in
                  (s::stack, [X86Call "read"; X86Mov (eax, s)])
              | S_WRITE  ->
                  let s::stack' = stack in
                  (stack', match s with
                  | S 0 -> [X86Call "write"]
                  | _   -> [X86Push s; X86Call "write"; X86Pop eax]
                  )
              | S_PUSH n ->
                  let s = allocate env stack in
                  (s::stack, [X86Mov (L n, s)])
              | S_LD x   ->
                  env#local x;
                  let s = allocate env stack in
                  (s::stack, match s with
                  | R _ -> [X86Mov (M x, s)]
                  | _   -> [X86Mov (M x, eax); X86Mov (eax, s)]
                  )
              | S_ST x   ->
                  env#local x;
                  let s::stack' = stack in
                  (stack', match s with
                  | R _ -> [X86Mov (s, M x)]
                  | _   -> [X86Mov (s, eax); X86Mov (eax, M x)]
                  )
                    | S_BINOP op ->
                  let y::x::stack' = stack in
                  (match op with
                  | "+" | "-" | "*" | "<=" | ">=" | "<" | ">" | "==" | "!=" ->
                      let preload, y' = (
                        match x, y with
                        | R _, _ | _, R _ ->
                            ([], y)
                        | _ ->
                            ([X86Mov (y, eax)], eax)
                      ) in
                      (x::stack', preload @
                      (match op with
                      | "+" -> [X86Add (y', x)]
                      | "-" -> [X86Sub (y', x)]
                      | "*" -> [X86Mul (y', x)]
                      | _ -> [
                        X86Mov (L 0, eax);
                        X86Cmp (y', x);
                        (match op with
                        | "==" -> X86SeteAl
                        | "!=" -> X86SetneAl
                        | "<"  -> X86SetlAl
                        | ">"  -> X86SetgAl
                        | "<=" -> X86SetleAl
                        | ">=" -> X86SetgeAl
                        );
                        X86Mov (eax, x)
                      ]
                      ))
                  | "&&" | "!!" -> (x::stack', [
                    X86Mov (L 0, eax);
                    X86Mov (L 0, edx);
                    X86Cmp (eax, x);
                    X86SetneDl;
                    X86Cmp (eax, y);
                    X86SetneAl;
                    if op = "&&" then X86And (edx, eax) else X86Or (edx, eax);
                    X86Mov (eax, x);
                  ])
                  | "/" | "%" ->
                      (x::stack', [
                        X86Mov (x, eax);
                        X86Cdq;
                        X86Div y;
                        X86Mov ((if op = "/" then eax else edx), x)])
                  )
            in
            x86code @ compile stack' code'
      in
      compile [] code

  end

let compile stmt =
  let env = new x86env in
  let code = Compile.stack_program env @@ Array.to_list @@ StackMachine.Compile.stmt stmt in
  let asm  = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in
  !"\t.text";
  List.iter (fun x ->
      !(Printf.sprintf "\t.comm\t%s,\t%d,\t%d" x word_size word_size))
    env#local_vars;
  !"\t.globl\tmain";
  let prologue, epilogue =
    if env#allocated = 0
    then (fun () -> ()), (fun () -> ())
    else
      (fun () ->
         !"\tpushl\t%ebp";
         !"\tmovl\t%esp,\t%ebp";
         !(Printf.sprintf "\tsubl\t$%d,\t%%esp" (env#allocated * word_size))
      ),
      (fun () ->
         !"\tmovl\t%ebp,\t%esp";
         !"\tpopl\t%ebp"
      )
  in
  !"main:";
  prologue();
  List.iter (fun i -> !(Show.instr i)) code;
  epilogue();
  !"\txorl\t%eax,\t%eax";
  !"\tret";
  Buffer.contents asm

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (compile stmt);
  close_out outf;
  let rc_runtime =
    (try Sys.getenv "RC_RUNTIME" with
    | Not_found -> "../runtime/runtime.o") in
  match Sys.command (Printf.sprintf "gcc -m32 -o %s %s %s.s" name rc_runtime name) with
  | 0 -> ()
  | _ -> failwith "gcc returned non-zero code"
