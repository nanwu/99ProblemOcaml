
let rec sort l = 
  match l with
    [] -> []
  | head :: tail -> insert head (sort tail)
and insert elem l = 
  match l with
    [] -> [elem]
  | head :: tail -> if elem < head then elem :: l else head :: insert elem tail;;

List.map print_int (sort [2; 5; 1; 7; 6]);;
