type opnd = R of int | S of int | M of string | L of int

let x86regs = [|
  "%ebx";
  "%ecx";
  "%esi";
  "%edi";
  "%eax";
  "%edx"
|]

let num_of_regs = Array.length x86regs
let last_free_reg = 4
let word_size = 4

let ebx = R 0
let ecx = R 1
let esi = R 2
let edi = R 3
let eax = R 4
let edx = R 5

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

type binop = Add | Sub | Mul | And | Or | Mov | Cmp
let binop_to_string = function
  | Add -> "addl"
  | Sub -> "subl"
  | Mul -> "imull"
  | And -> "andl"
  | Or  -> "orl"
  | Mov -> "movl"
  | Cmp -> "cmpl"

type instr =
  | X86Binop of binop * opnd * opnd
  | X86Div  of opnd
  | X86Cdq
  | X86Set  of set_suf * byte_reg
  | X86Push of opnd
  | X86Pop  of opnd
  | X86Ret
  | X86Call of string
  | X86Comm of string
  | X86Lbl  of string
  | X86Jmp  of string
  | X86Jz   of string

module S = Set.Make (String)

class x86env =
  object(self)
    val    last_allocated  = ref 0
    method allocate n = last_allocated := max n !last_allocated
    method cnt_allocated  = 1 + !last_allocated
  end

let allocate env stack =
  match stack with
  | []                                -> R 0
  | (S n)::_                          -> env#allocate (n+1); S (n+1)
  | (R n)::_ when n < last_free_reg-1 -> R (n+1)
  | _                                 -> S 0

module Show =
struct

  let opnd = function
    | R i -> x86regs.(i)
    | S i -> Printf.sprintf "-%d(%%ebp)" ((i+1) * word_size)
    | M x -> x
    | L i -> Printf.sprintf "$%d" i

  let instr = function
    | X86Binop (o, s1, s2) -> Printf.sprintf "\t%s\t%s,\t%s"
                                (binop_to_string o) (opnd s1) (opnd s2)
    | X86Div s2       -> Printf.sprintf "\tidivl\t%s"      (opnd s2)
    | X86Cdq          -> Printf.sprintf "\tcdq"
    | X86Set (suf, r) -> Printf.sprintf "\tset%s\t%%%s" (set_suf_to_string suf) (byte_reg_to_string r)
    | X86Push s       -> Printf.sprintf "\tpushl\t%s"      (opnd s )
    | X86Pop  s       -> Printf.sprintf "\tpopl\t%s"       (opnd s )
    | X86Ret          -> "\tret"
    | X86Call p       -> Printf.sprintf "\tcall\t%s" p
    | X86Comm s       -> Printf.sprintf "// %s" s
    | X86Lbl  l       -> Printf.sprintf "%s:" l
    | X86Jmp  l       -> Printf.sprintf "\tjmp\t%s" l
    | X86Jz   l       -> Printf.sprintf "\tjz\t%s" l

end

module Compile =
struct

  open StackMachine

  let stack_program env code =
    let rec compile stack code =
      match code with
      | []       -> []
      | (S_COMM c)::code' ->
        [X86Comm ("===== " ^ c ^ " =====")] @ compile stack code'
      | i::code' ->
        let (stack', x86code) =
          match i with
          | S_READ   ->
            let s = allocate env stack in
            (s::stack, [X86Call "read"; X86Binop (Mov, eax, s)])
          | S_WRITE  ->
            let s::stack' = stack in
            (stack', match s with
              | S 0 -> [X86Call "write"]
              | _   -> [X86Push s; X86Call "write"; X86Pop eax]
            )
          | S_PUSH n ->
            let s = allocate env stack in
            (s::stack, [X86Binop (Mov, L n, s)])
          | S_LD x   ->
            let s = allocate env stack in
            (s::stack, match s with
              | R _ -> [X86Binop (Mov, M x, s)]
              | _   -> [X86Binop (Mov, M x, eax); X86Binop (Mov, eax, s)]
            )
          | S_ST x   ->
            let s::stack' = stack in
            (stack', match s with
              | R _ -> [X86Binop (Mov, s, M x)]
              | _   -> [X86Binop (Mov, s, eax); X86Binop (Mov, eax, M x)]
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
                   ([X86Binop (Mov, y, edx)], edx)
               ) in
               (x::stack', preload @
                           (match op with
                            | "+" -> [X86Binop (Add, y', x)]
                            | "-" -> [X86Binop (Sub, y', x)]
                            | "*" -> (match x, y' with
                                | R _, _ -> [X86Binop (Mul, y', x)]
                                | _, R _ -> [X86Binop (Mul, x, y'); X86Binop (Mov, y', x)]
                              )
                            | _ -> [
                                X86Binop (Mov, L 0, eax);
                                X86Binop (Cmp, y', x);
                                X86Set (binop_to_set_suf op, Al);
                                X86Binop (Mov, eax, x)
                              ]
                           ))
             | "&&" | "!!" -> (x::stack', [
                 X86Binop (Mov, L 0, eax);
                 X86Binop (Cmp, L 0, x);
                 X86Set (Ne, Al);
                 X86Binop (Mov, L 0, edx);
                 X86Binop (Cmp, L 0, y);
                 X86Set (Ne, Dl);
                 if op = "&&" then X86Binop (And, edx, eax) else X86Binop (Or, edx, eax);
                 X86Binop (Mov, eax, x);
               ])
             | "/" | "%" ->
               (x::stack', [
                   X86Binop (Mov, x, eax);
                   X86Cdq;
                   X86Div y;
                   X86Binop (Mov, (if op = "/" then eax else edx), x)])
            )
          | S_LABEL l ->
            assert (stack = []);
            (stack, [X86Lbl l])
          | S_JMP l ->
            assert (stack = []);
            (stack, [X86Jmp l]);
          | S_JZ l ->
            let x::stack' = stack in
            assert (stack' = []);
            (stack', [X86Binop (Cmp, L 0, x); X86Jz l])
        in
        [X86Comm (StackMachine.i_to_string i)] @ x86code @ compile stack' code'
    in
    compile [] code

end

let compile stmt =
  let env = new x86env in
  let smcode = StackMachine.Compile.stmt stmt in
  let code = Compile.stack_program env @@ Array.to_list @@ smcode in
  let asm  = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in
  !"\t.text";
  List.iter (fun x ->
      !(Printf.sprintf "\t.comm\t%s,\t%d,\t%d" x word_size word_size))
    (StackMachine.used_vars smcode);
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
  !"// ===== START =====";
  List.iter (fun i -> !(Show.instr i)) code;
  !"// ===== END =====";
  epilogue();
  !"\txorl\t%eax,\t%eax";
  !"\tret";
  Buffer.contents asm

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (compile stmt);
  close_out outf;
  match Sys.command (Printf.sprintf "gcc -m32 -o %s $RC_RUNTIME/runtime.o %s.s" name name) with
  | 0 -> ()
  | _ -> failwith "gcc failed with non-zero exit code"
