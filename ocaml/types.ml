type 'a list =
    NE of 'a * 'a list
  | E

let rec length l =                    
  match l with
    E -> 0
  | NE(_, t) -> 1 + (length t)

let rec length1 = function
    E -> 0
  | NE(_, t) -> 1 + (length t)


(* Define your own binary tree. Two types of nodes: leaf and non-leaf.
number_of_nodes
is_there *)

type 'a bintree =
    NonLeaf of 'a * 'a bintree * 'a bintree
  | Leaf

(* 
  NonLeaf(1, 
     NonLeaf(  2,
               Leaf,
               NonLeaf( 3,
                        Leaf,
                        Leaf
                      )
            ), NonLeaf(
                        4,
                        NonLeaf(5, Leaf, Leaf),
                        NonLeaf(6, Leaf, Leaf)
                      )
           )
 *)

let t = 
  let t1 =
    let t2 = NonLeaf(3, Leaf, Leaf)
    in
    NonLeaf(2, Leaf, t2)
  and t3 = 
    let t4 = NonLeaf(5, Leaf, Leaf)
    and t5 = NonLeaf(6, Leaf, Leaf)
    in
    NonLeaf(4, t4, t5)
  in
  NonLeaf(1, t1, t3)
