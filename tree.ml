
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
let construct left right all =
  let add_right_tree all l = 
    List.fold_left (fun a r-> (Node('x', l, r ) :: a) all right 
  in List.fold_left add_right_tree all left 
   

let rec cabl_tree n = 
  if n = 0 then [Empty]
  else if n mod 2  = 1 then 
    let t = cabl_tree (n / 2) in
    construct t t
  else 
    let t1 = cabl_tree (n / 2) in 
    let t2 = cabl_tree (n / 2 - 1) in 
    construct t1 t2

