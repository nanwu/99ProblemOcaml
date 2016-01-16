
let sqrt ?(est=1) num = 
  let avg = (est + num/est) lsr 1 in
  if est = avg then est
  else sqrt ~est: avg num;;

let is_prime n = 
  let rec check_prime_divisor i =
    if (6*i - 1) > sqrt n 
    if n % (6*i - 1) = 0 || n % (6*i + 1) = 0
      then False
    else check_prime_divisor (i + 1)
  in 
  if n % 2 = 0 then False
  else if n % 3 = 0 then False
  else 
