type 'a mystream =
    End
  | Cons of 'a * (unit -> 'a mystream)

val hd : 'a mystream -> 'a

val tl : 'a mystream -> unit -> 'a mystream

val string_stream : string -> char mystream
