open Util

type opnd = R of int | S of int | A of int | L of int

let x86regs = [|
  "%ebx";
  "%ecx";
  "%esi";
  "%edi";
  "%eax";
  "%edx";
  "%esp";
  "%ebp"
|]

let num_of_regs = Array.length x86regs
let num_of_free_regs = 4
let word_size = 4
let regs_under_ebp = 3

let ebx = R 0
let ecx = R 1
let esi = R 2
let edi = R 3
let eax = R 4
let edx = R 5
let esp = R 6
let ebp = R 7

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

    val mutable vars : (string * opnd) list = []
    method set_vars l = vars <- l
    method get_var x = List.assoc x vars
  end

let allocate env stack =
  match stack with
  | []                                   -> R 0
  | (S n)::_                             -> env#allocate (n+1); S (n+1)
  | (R n)::_ when n < num_of_free_regs-1 -> R (n+1)
  | _                                    -> S 0

module Show =
struct

  let opnd = function
    | R i -> x86regs.(i)
    | S i -> Printf.sprintf "%d(%%ebp)" ((-regs_under_ebp - i - 1) * word_size)
    | A i -> Printf.sprintf "%d(%%ebp)" ((i+2) * word_size) (* +0 - old ebp, +1 - return address, +2 - args *)
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
          | S_PUSH n ->
            let s = allocate env stack in
            (s::stack, [X86Binop (Mov, L n, s)])
          | S_LD x   ->
            let x' = env#get_var x in
            let s = allocate env stack in
            (s::stack, match s with
              | R _ -> [X86Binop (Mov, x', s)]
              | _   -> [X86Binop (Mov, x', eax); X86Binop (Mov, eax, s)]
            )
          | S_ST x   ->
            let x' = env#get_var x in
            let s::stack' = stack in
            (stack', match s with
              | R _ -> [X86Binop (Mov, s, x')]
              | _   -> [X86Binop (Mov, s, eax); X86Binop (Mov, eax, x')]
            )
          | S_DROP ->
            let _::stack' = stack in
            (stack', [])
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
          | S_CALL (l, args_cnt) ->
            let (args, stack') = splitAt args_cnt stack in
            let x = allocate env stack' in
            (x::stack', List.concat [
              [X86Push ecx;
               X86Push edx];
              List.rev @@ List.map (fun n -> X86Push n) args;
              [X86Call l;
               X86Binop (Add, L (word_size * (List.length args)), esp);
               X86Pop edx;
               X86Pop ecx;
               X86Binop (Mov, eax, x)]
            ])
          | S_RET l ->
            let [x] = stack in
            ([], [X86Binop (Mov, x, eax); X86Jmp l])
          | S_FUN_BEGIN {args; locals; max_stack} ->
            assert (stack = []);
            let tmp_cnt = max 0 (max_stack - num_of_free_regs) in
            let args = List.mapi (fun i n -> (n, A i)) args in
            let locals = List.mapi (fun i n -> (n, S (tmp_cnt + i))) locals in
            let vars = args @ locals in
            env#set_vars vars;
            let comms = List.map (fun (n, o) -> X86Comm (Printf.sprintf "var %s -> %s" n (Show.opnd o))) vars in
            (* Two following blocks of code should be synchronized with regs_under_ebp *)
            (stack, [
                X86Push ebp;
                X86Binop (Mov, esp, ebp);
                X86Push ebx;
                X86Push esi;
                X86Push edi;
                X86Binop (Sub, L (word_size * (tmp_cnt + List.length locals)), esp);
              ] @ comms);
          | S_FUN_END ->
            assert (stack = []);
            (stack, [
                X86Binop (Mov, ebp, esp);
                X86Binop (Sub, L (word_size * regs_under_ebp), esp);
                X86Pop edi;
                X86Pop esi;
                X86Pop ebx;
                X86Pop ebp;
                X86Ret
              ]);
        in
        [X86Comm (StackMachine.i_to_string i)] @ x86code @ compile stack' code'
    in
    compile [] code

end

let compile prog =
  let env = new x86env in
  let smcode = StackMachine.Compile.prog prog in
  let code = Compile.stack_program env @@ Array.to_list @@ smcode in
  let asm  = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in
  !"\t.text";
  !"\t.globl\tmain";
  List.iter (fun i -> !(Show.instr i)) code;
  Buffer.contents asm

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (compile stmt);
  close_out outf;
  match Sys.command (Printf.sprintf "gcc -m32 -o %s $RC_RUNTIME/runtime.o %s.s" name name) with
  | 0 -> ()
  | _ -> failwith "gcc failed with non-zero exit code"
