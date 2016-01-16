
let sqrt ?(est=1) num = 
  let avg = (est + num/est) lsr 1 in
  if est = avg then est
  else sqrt ~est: avg num;;

let is_prime n = 
  let rec check_prime_divisor i =
    if (6*i - 1) > sqrt n then True
    else if n % (6*i - 1) = 0 || n % (6*i + 1) = 0 then False
    else check_prime_divisor (i + 1)
  in 
  if n % 2 = 0 then False
  else if n % 3 = 0 then False
  else check_prime_divisor 1 


let gcd x y = 
  if y = 0 then x
  else gcd y (x mod y)


let prime_factor n = 
  let rec helper d n =
    if n mod d = 0 then d :: helper d (n / d) else helper (d+1) n
  in helper 2 n


