exception End_of_buffer

let from_string s =
  let pos = ref 0 in
  let buffer () =
    if !pos = Bytes.length s then
      raise End_of_buffer
    else
    begin
      let c = s.[!pos]
      and lookahead =
        if !pos < (Bytes.length s) - 1 then
          Some(s.[!pos + 1])
        else
          None in
      begin
        pos := !pos + 1;
        (c, lookahead)
      end
    end
  in
  buffer

let from_file fname =
  let fin      = open_in fname in
  let lookahead = ref (Some(input_char fin))
  and curr_char = ref None in
  let buffer () =
    begin
    try
      if !lookahead = None then
        raise End_of_buffer
      else
        curr_char := !lookahead;
        lookahead := Some(input_char fin)
    with
      End_of_file ->
      begin
        close_in fin;
        lookahead := None;
      end
    end;
    match !curr_char with
      Some(c) -> (c, !lookahead)
    | None -> raise End_of_buffer
    
  in
  buffer

(*
let from_file fname =
  let fin      = open_in fname
  and line_pos = ref 0
  and line     = ref ""
  and line_num = ref 0 in
  let buffer () =
    try
      (* Has read past the end of line *)
      if !line_pos = String.length !line then
      begin
        if !line_num = 0 then
          line_pos := 0;
          line     := (input_line fin);
          line_num := !line_num + 1;
          let c = (!line).[!line_pos] in
          begin
            line_pos := !line_pos + 1;
            c
          end
        else
          line_pos := 0;
          line     := (input_line fin);
          line_num := !line_num + 1;
          let c = '\n'
          and lookahead = (!line).[!line_pos] in
          begin
            line_pos := !line_pos + 1;
            c
          end
         
      end
      else
      begin
        let c = (!line).[!line_pos] in
        begin
          line_pos := !line_pos + 1;
          c
        end
      end
    with
      End_of_file ->
      begin
        print_endline "EOF!";
        close_in fin;
        raise End_of_buffer
      end
  in
  buffer
*)
