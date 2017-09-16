type state =
    Terminate of bool
  | State of (char Mystream.mystream -> state)
