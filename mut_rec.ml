let rec is_odd n =       
    if n = 0 then false    
    else if n = 1 then true
    else is_even (n - 1)   
  
  and is_even n =
    if n = 0 then true
    else if n = 1 then false
    else is_odd (n - 1)


let ones l =
  let rec iter lst count =
    match lst with 
      [] -> count
    | h :: t when h = 1 -> (iter t (count + 1))
    | _ :: t -> (iter t count) 
 in
 iter l 0

let ones_mr l =
  let rec one l count =

  and two l count =

  in
  one l 0
