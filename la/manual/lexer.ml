exception Lexical_error

type token =
    NUM of float
  | ID  of string
  | IN

type tscanner = {
  tok    : token;
  state  : State.state; (* current state *)
  accept : (char -> char option -> State.state) list 
    (* set of accepting states *)
}
(* 
  string_of_<type> utility functions are often implemented to allow printing
   debugging
  information while testing and debugging your program.
*)
let string_of_token = function
    NUM(n) -> "NUM(" ^ (string_of_float n) ^ ")"
  | ID(id) -> "ID(" ^ id ^ ")"
  | IN -> "IN"

let string_of_scanner sc = "{ tok = " ^ (string_of_token sc.tok) ^ "; }"
(*
  lexer is an iterative function. The following record type summarises the
  data that gets maintained and updated over every iteration of lexer.
*)
type lexer_data = {
  current_token : token option; 
    (* The token that will be returned if scanning succeeds. *)
  lexeme        : string; 
    (* The corresponding part of the input string *)
  scanners      : tscanner list
    (* List of scanners corresponding to each token type *)
}

let string_of_scanners scanners =
  "[" ^ 
     (List.fold_left 
       (fun x sc -> x ^ ", " ^ (string_of_scanner sc))
       "" scanners
     ) ^
   "]"

let string_of_lexer_data data =
  "{ token = " ^ (
    match data.current_token with
      None -> "None"
    | Some(tok) -> string_of_token tok
  ) ^ "; "
  ^ "lexeme = "       ^ data.lexeme ^ "; "
  ^ "scanners = "     ^
    match data.scanners with
      [] -> "[]"
    | _  ->  (string_of_scanners data.scanners) ^ "; "
  ^ " }"

let string_of_char = String.make 1

(* Boiler plate selecter function *)
let filter_scanners (b1 : bool) (b2 : bool) (b3 : bool)
                    (scanners : tscanner list)
                    : tscanner list =
  List.filter (
      fun sc ->
        match sc.state with
          State.Terminate(true)  -> b1
        | State.Terminate(false) -> b2
        | _                      -> b3
     ) scanners

let select_success    = filter_scanners true false false

let select_continuers = filter_scanners false false true 

(* Process the current input character and lookahead character. It just
     replaces the current state of the given FSA with the next *)
let process_char (input : char)
                 (lookahead : char option) (sc : tscanner)
                 : tscanner =
  match sc.state with
    State.Terminate(_) -> failwith "Can't process terminated scanner."
  | State.State(fsa)   -> {sc with state = (fsa input lookahead)}
      (* return the token consumed and the new FSA state *)

(* 
  Every iteration of the lexer consumes one character from the input buffer.
  - All the FSAs in the scanners are exercised. Each of them will yield either
      of the following:
    * Proceed: The character is consumed and the next state is returned.
    * Terminate with failure: In this case, this FSA is removed from the
        scanners list for the next iteration.
    * Terminate with success.
  - The first of the FSAs which terminate with success is chosen to update the
      current_token and lexeme.

  The lexer must return if any of the following conditions arises:
  - The list of scanners becomes empty, i.e. all scanners have terminated.
  - We reach the end of the input buffer.
  In that case, if any of the currently live scanners is in an accepting state
    the first of those is returned. Otherwise, failure is reported.
*)
let lexer (buffer : unit -> (char * char option)) : token =
  let rec iter (data : lexer_data) : lexer_data =
    match data.scanners with
      [] -> data (* if no scanners are passed from the previous iteration, 
                    return the current data containing the longest token
                    recognised so far *)
    | _  ->
      let (new_char, lookahead)    = buffer () in
      let new_fsas   = List.map
                        (process_char new_char lookahead) data.scanners in
      let successes  = select_success    new_fsas
      and continuers = select_continuers new_fsas in
      let (new_token, new_lexeme) =
        (*
           If any of the scanners terminated successfully for the the current
             character,
           - update the current_token with the token corresponding to the first
             of the successfully terminated scanners
           - the lexeme to the current input_string
           The above corresponds to the lexing policy of preferring the longest
            possible token that can be recognised. It should be noted that this
            policy can be changed depending on the specific case.
           It is to keep this flexibility that the FSAs have been implemented
            with lazy evaluation.
        *)
        if continuers = [] then
          if successes <> [] then
            let sc = List.hd successes in
            (
              Some(sc.tok),
              data.lexeme ^ (string_of_char new_char)
            )
          else raise Lexical_error
        else
          (
            None,
            data.lexeme ^ (string_of_char new_char)
          )
      in
      let new_data = {
          current_token = new_token;
          lexeme        = new_lexeme;
          scanners      = continuers
        } in
     iter new_data (* next iteration *)
  in
  (* initialisation *)
  (*
  The lexer begins by:
  - setting the current_token to None, since nothing has been detected so far.
  - lexeme to ""
  - scanners to FSAs corresponding to all the token types.
  *)
  let st = iter {
      current_token = None; (* No token has been successfully read so far. *) 
      lexeme        = "";   (* No lexeme has been successfully read so far. *)
      scanners      = List.map 
                       (
                         fun (tok, st) -> let i, a = st () in
                           { tok = tok; state = i; accept = a }
                       )
                       [ (NUM(0.), Num.num); (IN, In.keywd_in); (ID(""), Id.id)
                      ]
     (*
       Each token class with its scanner to begin with. To make this code work
         with additional token types, we just need to add those elements into
         this list. It is, however, important to order the scanners
         appropriately to ensure correction identification of tokens. For
         example, for an input string "in", both IN and ID will accept. But IN
         should be chosen. Since, lexer is configured to choose the output of
         the first of the successful scanners, it is important that it selects
         IN, and not ID. Hence, IN must appear before ID in the scanners list.
     *)
    } in
    match st.current_token with
      (* iter couldn't scan any token successfully *)
      None -> failwith "Lexical error"
    | Some(tok) ->
       match tok with
         NUM(_) -> NUM(float_of_string st.lexeme)
       | ID(_)  -> ID(st.lexeme)
       | IN  -> IN
