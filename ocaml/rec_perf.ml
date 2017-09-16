let rec max  = function
    [] -> -1
  | h :: t -> let maxt = (max t) in
                if h > maxt then h else maxt
