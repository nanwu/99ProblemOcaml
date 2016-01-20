
type 'a binary_tree = 
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;


(* Check if a binary tree symmetric or not *)
let is_mirror n1 n2 = 
  match n1 n2 with
  | Empty, Empty -> true
  | Node(v1, l1, r1) Node(v2, l2, r2) -> v1 == v2
                                         && is_mirror l1, r2
                                         && is_mirror l2, r1
  | _ -> false     

let is_symmetric root = 
  | empty -> True
  | Node(_, l, r) -> is_mirror l r


(* Construct completely balanced binary trees. *)
let build_tree_from 
let rec cabl_tree n = 
  if n = 0 then [Empty]
  else if n mod 2  = 1 then 
    let t = cabl_tree (n / 2) in
    
    | x -> if (x mod 2) = 0 then aux 

