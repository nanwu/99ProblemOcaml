

let rec gray n = 
  if n = 1 then ["0"; "1"]
  else let g = gray (n-1) in
       let prep c s = c ^ s in
       List.map (prep "0") g @ List.rev_map (prep "1") g;;

let three = gray 2;;
List.map print_endline three;;
