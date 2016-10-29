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

type byte_reg = Al | Dl
type set_suf = E | Ne | L | G | Le | Ge
let binop_to_set_suf op =
  match op with
  | "==" -> E
  | "!=" -> Ne
  | "<"  -> L
  | ">"  -> G
  | "<=" -> Le
  | ">=" -> Ge
let set_suf_to_string suf = match suf with
  | E -> "e" | Ne -> "ne"
  | L -> "l" | Le -> "le"
  | G -> "g" | Ge -> "ge"
let byte_reg_to_string r = match r with
  | Al -> "al" | Dl -> "dl"

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
  | X86Set  of set_suf * byte_reg
  | X86Push of opnd
  | X86Pop  of opnd
  | X86Ret
  | X86Call of string
  | X86Comm of string

module S = Set.Make (String)

class x86env =
  object(self)
    val    local_vars = ref S.empty
    method local x    = local_vars := S.add x !local_vars
    method local_vars = S.elements !local_vars

    val    last_allocated  = ref 0
    method allocate n = last_allocated := max n !last_allocated
    method cnt_allocated  = 1 + !last_allocated
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
    | X86Set (suf, r) -> Printf.sprintf "\tset%s\t%%%s" (set_suf_to_string suf) (byte_reg_to_string r)
    | X86Push s       -> Printf.sprintf "\tpushl\t%s"      (opnd s )
    | X86Pop  s       -> Printf.sprintf "\tpopl\t%s"       (opnd s )
    | X86Ret          -> "\tret"
    | X86Call p       -> Printf.sprintf "\tcall\t%s" p
    | X86Comm s       -> Printf.sprintf "// %s" s

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
                   ([X86Mov (y, edx)], edx)
               ) in
               (x::stack', preload @
                           (match op with
                            | "+" -> [X86Add (y', x)]
                            | "-" -> [X86Sub (y', x)]
                            | "*" -> (match x, y' with
                                | R _, _ -> [X86Mul (y', x)]
                                | _, R _ -> [X86Mul (x, y'); X86Mov (y', x)]
                              )
                            | _ -> [
                                X86Mov (L 0, eax);
                                X86Cmp (y', x);
                                X86Set (binop_to_set_suf op, Al);
                                X86Mov (eax, x)
                              ]
                           ))
             | "&&" | "!!" -> (x::stack', [
                 X86Mov (L 0, eax);
                 X86Cmp (L 0, x);
                 X86Set (Ne, Al);
                 X86Mov (L 0, edx);
                 X86Cmp (L 0, y);
                 X86Set (Ne, Dl);
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
        [X86Comm (StackMachine.i_to_string i)] @ x86code @ compile stack' code'
    in
    compile [] code

end

let compile stmt =
  let env = new x86env in
  let code = Compile.stack_program env @@ StackMachine.Compile.stmt stmt in
  let asm  = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in
  !"\t.text";
  List.iter (fun x ->
      !(Printf.sprintf "\t.comm\t%s,\t%d,\t%d" x word_size word_size))
    env#local_vars;
  !"\t.globl\tmain";
  let prologue, epilogue =
    if env#cnt_allocated = 0
    then (fun () -> ()), (fun () -> ())
    else
      (fun () ->
         !"\tpushl\t%ebp";
         !"\tmovl\t%esp,\t%ebp";
         !(Printf.sprintf "\tsubl\t$%d,\t%%esp" (env#cnt_allocated * word_size))
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
