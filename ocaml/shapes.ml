type shape =
    Rectangle of float * float
  | Circle of float

let area = function
    Rectangle(l, b) -> l *. b
  | Circle(r) -> 3.141 *. r *. r


let perimeter = function
    Rectangle(l, b) -> 2. *. (l +. b)
  | Circle(r) -> 2. *. 3.141 *. r
