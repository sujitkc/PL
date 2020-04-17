type state =
    Terminate of bool
  | State of (char -> char option -> state)
