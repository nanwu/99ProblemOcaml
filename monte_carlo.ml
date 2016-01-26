
let get_pi radius = 
  let in_circle x y = 
    sqrt (float_of_int (x * x + y * y)) <= float_of_int radius
  in 
  let has_hit unit = 
    in_circle (Random.int (2 * radius + 1) - radius) (Random.int (2 * radius + 1) - radius)
  in
  let rec next hit sofar total = 
    if sofar = total then float_of_int hit /. float_of_int sofar *. 4.0
    else if has_hit () then next (hit+1) (sofar+1) total
         else next hit (sofar+1) total
  in next 0 0 1000000;;

print_float (get_pi 10000000);;

