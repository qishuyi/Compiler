type token =
  | TInt of int
  | TLParen
  | TRParen
  | TPlus
  | TMinus
  | TMultiply
  | TDivide
  | TBoolean of bool
  | TLessthanorequal
  | TIf
  | TFloat of float
  | TNan

let string_of_token (t:token) : string =
  match t with
  | TInt n  -> string_of_int n
  | TLParen -> "("
  | TRParen -> ")"
  | TPlus   -> "+"
  | TMinus -> "-"
  | TMultiply -> "*"
  | TDivide -> "/"
  | TBoolean b -> string_of_bool b
  | TLessthanorequal -> "<="
  | TIf -> "if"
  | TFloat f -> string_of_float f
  | TNan -> string_of_float nan

let string_of_token_list (toks:token list) : string =
  "[" ^ (String.concat ", " (List.map string_of_token toks)) ^ "]"

(* Peeks at the head of the stream without advancing it forward *)
let peek (src:char Stream.t) : char =
  match Stream.peek src with
  | Some ch -> ch
  | None    -> failwith "Unexpected end of file encountered"

(* Pops the head of the stream and returns it, advancing the stream forward *)
let advance : char Stream.t -> char = Stream.next

(* Returns true iff this stream still has elements left *)
let is_empty (src:char Stream.t) : bool =
  try
    Stream.empty src; true
  with
    Stream.Failure -> false

let is_whitespace (ch:char) : bool =
  ch = ' ' || ch = '\012' || ch = '\n' || ch = '\r' || ch = '\t'

let is_digit (ch:char) : bool =
  let code = Char.code ch in
  48 <= code && code <= 57

(* Note: lex contains two nested helper functions, lex_num and go *)
let lex (src:char Stream.t) : token list =
let rec lex_num acc =
  let ch = peek src in
    if is_digit ch then
      lex_num (acc ^ (Char.escaped (advance src)))
    else if ch = '.' then
      begin let cur_char = advance src in let next_char = peek src in
      if is_digit (String.get acc ((String.length acc) - 1)) && 
         is_digit next_char && not (String.contains acc '.')
      then lex_num (acc ^ (Char.escaped cur_char))
      else failwith "A floating-point literal is a number with a decimal point. And there exists at least one decimal to the left and right of the decimal point."    end
    else acc
  in
  let rec go () =
    if not (is_empty src) then
      let ch = peek src in
      (* Note: the |> operator takes the result of the left-hand side
       * and feeds it as an argument to the function on the right-hand
       * side.  ignore has type 'a -> unit---it allows us to throw
       * away the return type of a function we don't care about *)
      match ch with
      | '(' -> advance src |> ignore; TLParen :: go ()
      | ')' -> advance src |> ignore; TRParen :: go ()
      | '+' -> advance src |> ignore; TPlus :: go ()
      | '-' -> advance src |> ignore; TMinus :: go()
      | '*' -> advance src |> ignore; TMultiply :: go()
      | '/' -> advance src |> ignore; TDivide :: go()
      | '<' -> advance src |> ignore ; 
               if advance src = '=' then TLessthanorequal :: go() 
               else failwith "Wrong Syntax. Hint: Maybe you meant '<='?"
      | 't' -> advance src |> ignore; 
               let ch1 = advance src in 
               let ch2 = advance src in 
               let ch3 = advance src in 
               if ch1 = 'r' && ch2 = 'u' && ch3 = 'e' then 
               begin ignore ch1; ignore ch2; ignore ch3; 
                 TBoolean true :: go() end
               else failwith "Wrong expression. Hint: Maybe you meant 'true'?"
      | 'f' -> advance src |> ignore;
               let ch1 = advance src in
               let ch2 = advance src in
               let ch3 = advance src in
               let ch4 = advance src in
               if ch1 = 'a' && ch2 = 'l' && ch3 = 's' && ch4 = 'e' then
               begin ignore ch1; ignore ch2; ignore ch3; ignore ch4; 
                 TBoolean false :: go() end
               else failwith "Wrong expression. Hint: Maybe you meant 'false'?"
      | 'i' -> advance src |> ignore;
               if advance src = 'f' then TIf :: go() 
               else failwith "Wrong Syntax. Hint: Maybe you meant 'if'?"
      | 'n' -> advance src |> ignore;
               let ch1 = advance src in
               let ch2 = advance src in
               if ch1 = 'a' && ch2 = 'n' then
               begin ignore ch1; ignore ch2; TNan :: go() end
               else failwith "Wrong expression. Hint: Maybe you meant 'nan'?"
      | _   ->
        if is_whitespace ch then
          begin advance src |> ignore; go () end
        else if is_digit ch then
          let n = lex_num "" in begin
            if (String.contains n '.')
            then TFloat (float_of_string n) :: go()
            else TInt (int_of_string n) :: go () end
        else if ch = '.' then failwith "In a floating point literal, there should be at least one digit to the left and to the right of the decimal point."
        else
          failwith (Printf.sprintf "Unexpected character found: %c" ch)
    else
      []
  in
    go ()
