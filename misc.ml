(* Eight Queen problem*)
let possible row col used_cols usedD1 usedD2 = 
  not (List.mem col used_cols
       || List.mem (row + col) usedD1
       || List.mem (row - col) usedD2

let queens_position n = 
  let rec aux row col used_cols usedD1 usedD2 = 
    if row > n then [used_cols]
    else (if col < n then aux row (col+1) used_cols usedD1 usedD2
          else []) @
         (if possible row col used_cols usedD1 usedD2 then
            aux (row+1) 1 (col :: used_cols) (row+col :: usedD1) (row-col :: usedD2)
          else [])
  in aux 1 1 [] [] []
  
