type typ =
    Boolean
  | Integer

(* Expression - begin *)
type expr =
  | Id        of string
  | IntConst  of int
  | Add       of expr * expr
  | Sub       of expr * expr
  | BoolConst of bool
  | Not       of expr
  | Or        of expr * expr
  | And       of expr * expr
  | Equals    of expr * expr
(* Expression - end *)

type stmt =
  | Assignment of string * expr
  | If of expr * stmt * stmt
  | Loop of expr * stmt
  | Seq of stmt list

type program = {
  s : stmt;
}

type cfgnode =
    Inode of stmt
  | Dnode of expr * cfgnode * cfgnode
  | Entry
  | Exit


type cfg = {
  nodes : cfgnode list;
  edges : (cfgnode * cfgnode) list;
  entry : cfgnode;
  exit : cfgnode;
}

let rec string_of_expr = function
  | Id(vname)          -> vname
  | IntConst(n)        -> string_of_int n
  | Add(e1, e2)        ->
      (string_of_expr e1) ^ " + " ^ (string_of_expr e2)
  | Sub(e1, e2)        ->
      (string_of_expr e1) ^ " - " ^ (string_of_expr e2)
  | BoolConst(b)       -> string_of_bool b
  | Not(e)             ->
      "not(" ^ string_of_expr e ^ ")"
  | Or(e1, e2)         ->
      "(" ^ (string_of_expr e1) ^ ") or (" ^ 
      (string_of_expr e2) ^ ")"
  | And(e1, e2)        -> 
      "(" ^ (string_of_expr e1) ^ ") and (" ^
      (string_of_expr e2) ^ ")"
  | Equals(e1, e2)     -> "(" ^
      (string_of_expr e1) ^ ") = (" ^ (string_of_expr e2) ^ ")"

let rec string_of_stmt = function
  | Assignment(s, e) ->
      s ^ " := " ^ (string_of_expr e) ^ ";"
  | If(b, s1, s2)      ->
      "if (" ^(string_of_expr b) ^ ") then (" ^
      (string_of_stmt s1) ^ ") else (" ^
      (string_of_stmt s2) ^ ")"
  | Loop(b, s)      ->
      "while (" ^(string_of_expr b) ^ ") do (" ^
      (string_of_stmt s) ^ ")"
  | Seq(slist) -> "{\n" ^
      (List.fold_left (fun x y -> x ^
      (string_of_stmt y) ^ "\n") "" slist) ^ "}"

let string_of_program p = string_of_stmt p.s

let join cfg1 cfg2 =
  {
      nodes = cfg1.nodes @ cfg2.nodes;
      edges = cfg1.edges @ cfg2.edges @ [(cfg1.exit, cfg2.entry)];
      entry = cfg1.entry;
      exit = cfg2.exit;
  }

let rec make_CFG_stmt s =
  match s with
    Assignment(_, _) -> let i = Inode(s) and n = Entry and x = Exit in
    {
      nodes = [n; i; x];
      edges = [(n, i); (i, x)];
      entry = n;
      exit = x;
    }
  | If(c, s1, s2) ->
      let i1 = (make_CFG_stmt s1) and i2 = (make_CFG_stmt s2) in
      let d = Dnode(c, i1.entry, i2.entry)
      and n = Entry and x = Exit in 
    {
      nodes = [n; d; x] @ i1.nodes @ i2.nodes;
      edges = [(n, d); (d, i1.entry); (d, i2.entry); (i1.exit, x); 
                (i2.exit, x)]
              @ i1.edges @ i2.edges;
      entry = n;
      exit = x;
    }
  | Loop(c, s) ->
      let i = (make_CFG_stmt s) and x = Exit in
      let d = Dnode(c, i.entry, x)
      and n = Entry in 
    {
      nodes = [n; d; x] @ i.nodes;
      edges = [(n, d); (d, i.entry); (i.exit, d); (d, x)] @ i.edges;
      entry = n;
      exit = x;
    }
  | Seq(slist) ->
      (*            
      let n = Entry and x = Exit in
      let emptyCFG = {
        nodes = [n; x]; edges = [(n, x)]; entry = n; exit = x;
      } in
      *)
      let cfgs = List.map make_CFG_stmt slist in
      match cfgs with
        h :: t -> List.fold_left join h t
      | [] -> failwith "make_CFG_stmt : Empty sequence of statements"
    
let make_CFG_prog (p : program) = make_CFG_stmt p.s

(*
i := 0
x := 1
y := 5
while (x = y) do
  x := y + 1
  if (y = 5) then
    z := x + y
  else
    z := x - y
    i := i + 1
  endif
done
z := x + y
*)
let p1 =
  let ss1 =
    let s1 = Assignment("i", IntConst(0))
    and s2 = Assignment("x", IntConst(1))
    and s3 = Assignment("y", IntConst(5))
    and s4 =
      let c =
        let l = Id("x") and r = Id("y")
      in
      Equals(l, r)
      and b =
        let s1 = Assignment("x", Add(Id("y"), IntConst(1)))
        and s2 = 
          let c =
            let l = Id("y") and r = IntConst(5) in
            Equals(l, r)
          and s1 = Assignment("x", Add(Id("y"), IntConst(1))) 
          and s2 =
            let s1 = Assignment("x", Sub(Id("y"), IntConst(1)))
            and s2 = Assignment("i", Add(Id("i"), IntConst(1)))
          in
          Seq([s1; s2])
        in
        If(c, s1, s2)
        in
      Seq([s1; s2])
    in Loop(c, b)
    and s5 = Assignment("z", Add(Id("x"), Id("y")))
    in
    Seq([s1; s2; s3; s4; s5;])     
  in
  { s = ss1 }

(*
i := 0
x := 1
y := 5
*)
let p2 =
  let ss1 =
    let s1 = Assignment("i", IntConst(0))
    and s2 = Assignment("x", IntConst(1))
    and s3 = Assignment("y", IntConst(5))
    in
    Seq([s1; s2; s3])     
  in
  { s = ss1 }

let t1 () =
  print_endline (string_of_program p1)

let t2 () = make_CFG_prog p1

let t3 () = make_CFG_prog p2

let main () =
  t1 ()

let _ = main ()
