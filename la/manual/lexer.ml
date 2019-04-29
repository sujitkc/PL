type token =
    NUM of float
  | ID  of string
  | IN

(* 
  string_of_<type> utility functions are often implemented to allow printing debugging
  information while testing and debugging your program.
*)
let string_of_token = function
    NUM(n) -> "NUM(" ^ (string_of_float n) ^ ")"
  | ID(id) -> "ID(" ^ id ^ ")"
  | IN -> "IN"

(*
  lexer is an iterative function. The following record type summarises the
  data that gets maintained and updated over every iteration of lexer.
*)
type lexer_data = {
  current_token : token option;              (* The token that will be returned if scanning succeeds.               *)
  lexeme        : string;                    (* The corresponding part of the input string                          *)
  input_string  : string;                    (* Part of the input string which has been read so far from the start. *)
  current_input : char Mystream.mystream;    (* The stream representing the input                                   *)
  scanners      : (token * State.state) list (* List of scanners corresponding to each token type                   *)
}

let string_of_scanners scanners =
  "[" ^ (List.fold_left (fun x (tok, _) -> x ^ ", " ^ (string_of_token tok)) "" scanners) ^ "]"

let string_of_lexer_data data =
  "{ token = " ^ (
    match data.current_token with
      None -> "None"
    | Some(tok) -> string_of_token tok
  ) ^ "; "
  ^ "lexeme = "       ^ data.lexeme ^ "; "
  ^ "input_string = " ^ data.input_string ^ "; "
  ^ "scanners = "     ^
    match data.scanners with
      [] -> "[]"
    | _  ->  (string_of_scanners data.scanners) ^ "; "
  ^ " }"

let string_of_char = String.make 1

(* Boiler plate selecter function *)
let filter_scanners (b1 : bool) (b2 : bool) (b3 : bool) (scanners : (token * State.state) list) : (token * State.state) list =
  List.filter (
      fun (_, st) ->
        match st with
          State.Terminate(true)  -> b1
        | State.Terminate(false) -> b2
        | _                      -> b3
     ) scanners

let select_success    = filter_scanners true false false

let select_continuers = filter_scanners false false true 

(* Process the first character in the input stream. It just replaces the current state of the given FSA with the next *)
let process_char (input : char Mystream.mystream) ((tok, st) : token * State.state) : token * State.state =
(*  print_string ("char = " ^ (string_of_char (Mystream.hd input)) ^ "\n"); *)
  match st with
    State.Terminate(_) -> failwith "Can't process terminated scanner."
  | State.State(fsa)   -> (tok, (fsa input)) (* return the token consumed and the new FSA state *)

(* 
  The lexer begins by:
  - setting the current_token to None, since nothing has been detected so far.
  - lexeme to ""
  - input_string to ""
  - input to the input stream
  - scanners to FSAs corresponding to all the token types.

  Every iteration of the lexer consumes one character from the input stream.
  - All the FSAs in the scanners are exercised. Each of them will yield either of the following:
    * Proceed: The character is consumed and the next state is returned.
    * Terminate with failure: In this case, this FSA is removed from the scanners list for the next iteration.
    * Terminate with success.
  - The first of the FSAs which terminate with success is chosen to update the current_token and lexeme.

  The lexer must return if any of the following conditions arises:
  - The list of scanners becomes empty, i.e. all scanners have terminated.
  - The input becomes empty.
  In that case, if the current_token does have some token in it, this is returned. Otherwise, failure is reported.
*)
let lexer (s : (char Mystream.mystream)) : token * (char Mystream.mystream) =
  let rec iter (data : lexer_data) : lexer_data =
    match data.scanners with
      [] -> data (* if no scanners are passed from the previous iteration, 
                    return the current data containing the longest token recognised so far *)
    | _  -> 
      let new_fsas   = List.map (process_char data.current_input) data.scanners in
      let successes  = select_success    new_fsas
      and continuers = select_continuers new_fsas
      and new_char   = (string_of_char (Mystream.hd data.current_input)) in
      let (new_token, new_lexeme) =
        (*
           If any of the scanners terminated successfully for the the current character,
           - update the current_token with the token corresponding to the first of the successfully 
           terminated scanners
           - the lexeme to the current input_string
           The above corresponds to the lexing policy of preferring the longest possible token that can be
           recognised. It should be noted that this policy can be changed depending on the specific case.
           It is to keep this flexibility that the FSAs have been implemented with lazy evaluation.
        *)
        if successes = [] then (data.current_token, data.lexeme)
        else
          let (nt, _) = List.hd successes in
          (
            Some(nt),
            data.input_string ^ new_char
          )
      and new_string = data.input_string ^ new_char
      in
      let new_data = {
          current_token = new_token;
          lexeme        = new_lexeme;
          input_string  = new_string;
          current_input = ((Mystream.tl data.current_input) ());
          scanners      = continuers
        } in
     iter new_data (* next iteration *)
  in
  (* initialisation *)
  let st = iter {
      current_token = None; (* No token has been successfully read so far. *)
      lexeme        = "";   (* No lexeme has been successfully read so far. *)
      input_string  = "";   (* Part of the input stream that has been read so far is of course empty in the beginning. *)
      current_input = s;    (* Current input is nothing but the head of the input stream. *)
      scanners      = [ (NUM(0.), State.State(Num.num)); (IN, State.State(In.keywd_in)); (ID(""), State.State(Id.id)) ]
                            (* Each token class with its scanner to begin with. To make this code work with additional token
                               types, we just need to add those elements into this list. It is, however, important to order
                               the scanners appropriately to ensure correction identification of tokens. For example, for 
                               an input string "in", both IN and ID will accept. But IN should be chosen. Since, lexer is
                               configured to choose the output of the first of the successful scanners, it is important
                               that it selects IN, and not ID. Hence, IN must appear before ID in the scanners list. *)
    } in
    match st.current_token with
      None -> failwith "Lexical error" (* iter couldn't scan any token successfully *)
    | Some(tok) ->
       match tok with
         NUM(_) -> (NUM(float_of_string st.lexeme), st.current_input)
       | ID(_)  -> (ID(st.lexeme), st.current_input)
       | IN  -> (IN, st.current_input)
