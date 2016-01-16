

let rand_select l num = 
  let res = Array.make num 0 
  in
  let update m = 
    let x = Random.int (m+1) in 
    if x < num then res.(x) <- (List.nth l m)
  in
  let rec aux m n = 
    if m = n then res
    else begin update m; aux (m+1) n end
  in aux 0 (List.length l);;

let selected = rand_select [1; 2; 3; 4; 5; 6] 4;;
Array.map print_int selected;;
