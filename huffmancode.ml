(*
 * A Huffman encoder implemented in OCaml. 
 * Explanation about Huffman encoding: https://en.wikipedia.org/wiki/Huffman_coding
 *
 * Author: Nan Wu
 * Date: 01/15/2016
*)


module PrioQueue = struct
  
  type 'a 'b v = {priority : 'a; value: 'b}

  type v heap = 
    | Node of v * v heap * v heap * int
    | Leaf

  let rec insert x : v = function
    | Leaf -> Node (x, Leaf, Leaf, 0)
    | Node (y, l, r, n) -> 
      let (stay, move) = if x.priority <= y.priority then (y, x) else (x, y)
      in 
      match (l, r) with
        | (Leaf, Leaf) -> Node (stay, Node (move, Leaf, Leaf, 0), Leaf, 1)
        | (Leaf, _) -> Node (stay, Node (move, Leaf, Leaf, 0), Leaf, n+1)
        | (_, Leaf) -> Node (stay, Leaf, Node (move, Leaf, Leaf, 0), n+1)
        | (Node (_, _, _, nl), Node (_, _, _, nr)) -> 
          if nl <= nr then
            Node (stay, (insert x l), r, n+1)
          else
            Node (stay, l, (insert x r), n+1)
end




