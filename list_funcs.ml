
type 'a elem = One of 'a | Many of 'a elem list;;

let rec flatten l = 
  match l with
  | [] -> []
  | One a :: tl -> a :: flatten tl
  | Many a :: tl -> (flatten a) @ flatten tl;;

(* Eliminate consecutive duplicates of list elements. *)
let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | s -> s

(* Pack consecutive duplicates of list elements into sublists. *)



(* Run-length encoding of a list *)
let encode l = 
  let rec aux acc count = function
    | [] -> []
    | [x] -> (count+1, x) :: acc
    | x :: [y :: _ as tl] -> if x = y: aux acc (count+1) tl
                             else (count+1, x) :: aux acc 0 tl;;

let encoded = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;


              
    
