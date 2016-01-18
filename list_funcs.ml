
type 'a elem = Single of 'a | Multi of ('a elem list);;

let rec flatten l = 
  match l with
  | [] -> []
  | hd :: tl -> match hd with
                | Single x -> x :: flatten tl
                | Multi x -> (flatten x) @ flatten tl;;


let node1 = Single 1;;
let node2 = Multi [Single 2; Single 3; Single 4];;
let node3 = Single 5;;
let flattened = flatten [node1; node2; node3];;
List.map print_int flattened;;

