type expr =
    Add of expr * expr
  | Const of int
  | FunApp of string * expr list
  | Var of int

type stmt =
    Assign of expr * expr
  | Expr of expr
  | Return of expr
  | StmtLst of stmt list


type def =
    FunDef of string * int * stmt

module Program = struct
  type program = {
    defs : def list;
    main : stmt
  }
end

exception CodeGenError of string

let push_reg r =
"
        sw      " ^ r ^ " 0($sp) # push " ^ r ^ "(step 1)" ^
"
    	addiu	$sp $sp -4 # push " ^ r ^ "(step 2)
"

let pop_reg r =
"
        lw      " ^ r ^ " 4($sp) # pop " ^ r ^ "(step 1)" ^
"
    	addiu	$sp $sp 4  # pop " ^ r ^ "(step 2)
"

let common_fun_starter = 
"
	move	$fp $sp
" ^ (push_reg "$ra")

let common_fun_ender =
  (pop_reg "$ra") ^
"
        move    $sp $fp     # restore stack pointer to the beginning of the current stack frame
        lw      $fp 0($sp)  # restore frame pointer to its previous value
        jr      $ra         # jump back to the caller
" 

let open_up () =
"
    .data
    .text

f_print:
"
^ common_fun_starter ^
"
        lw      $a0 4($fp)
        li      $v0 1
        syscall
"
^ common_fun_ender

let close_down () =
"
        li      $v0 10
        syscall
"

let cgen_lvalue = function
  | Var(vnum) ->
"
        sw      $a0 " ^ (string_of_int (4 * vnum)) ^ "($fp)\n"
  | _ -> raise (CodeGenError "cgen_lvalue: only variable type expression accepted.")

let rec cgen_expr = function
    Add(e1, e2) ->
      cgen_expr(e1)    ^
      (push_reg "$a0") ^
      cgen_expr(e2)    ^
      (pop_reg "$t1")  ^
"      
        addu    $a0 $a0 $t1
"
  | Const(n) ->
"
        li      $a0 " ^ (string_of_int n) ^ "\n"
  | FunApp(fname, args) ->
"
        # function call sequence - begin
" ^ (push_reg "$fp") ^ (cgen_args args) ^
"
        jal f_" ^ fname ^
"
        # function call sequence - end
"
  | Var(vnum) -> 
"
        lw      $a0, " ^ (string_of_int (4 * vnum)) ^ "($fp)\n"

and cgen_args args =
  let cgen_arg arg =
    cgen_expr arg ^ (push_reg "$a0")
  in
  let rec iter = function
    [] -> ""
  | arg :: args -> (cgen_arg arg) ^ (cgen_args args)
  in
  iter (List.rev args)
  
let rec cgen_stmt = function
    Assign(e1, e2) -> (cgen_expr e2) ^ (cgen_lvalue e1)
  | Expr(e) -> cgen_expr(e)
  | Return(e) -> cgen_expr(e) ^ common_fun_ender
  | StmtLst(lst) ->
    (
      List.fold_left (fun a b -> a ^ b) "" (List.map cgen_stmt lst)
    )

let cgen_def = function
    FunDef(fname, args, e) -> "f_" ^ fname ^ ":\n"
    ^ common_fun_starter ^ (cgen_stmt e)

let rec cgen_defs = function
    [] -> ""
  | h :: t -> (cgen_def h) ^ (cgen_defs t)

let cgen_prog p =
    cgen_defs p.Program.defs ^
"
main:
       move $fp $sp
" 

  ^ cgen_stmt p.Program.main
    
let rec cgen p =
    open_up ()
  ^ cgen_prog p
  ^ close_down ()

(*

add(x, y) {
  return x + y;
}

x = y + 2;
print(10);
*)
let p1 =
  let f1 =
    let e = Return(Add(Var(1), Var(2))) in
    FunDef("add", 2, StmtLst([e]))
  and s1 =
        Expr(FunApp("add", [Const(2); Const(2)]))
  and s2 =
      Expr(FunApp(
        "print",
        [Const(10)]
      ))
  in
  {
    Program.defs = [f1];
    Program.main = StmtLst([s1; s2])
  }

let _ = print_string (cgen p1)
