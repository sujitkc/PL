exception End_of_buffer
val from_string : string -> (unit -> (char * char option))
val from_file : string -> (unit -> (char * char option))
