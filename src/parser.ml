open Lang
open Lexer

let rec peek : token list -> token = List.hd
let rec advance : token list -> token list = List.tl

let rec consume (t:token) (toks:token list) : token list =
  match toks with
  | t' :: toks ->
    if t = t' then
      toks
    else
      failwith (Printf.sprintf "Expected '%s', found '%s'" (string_of_token t) (string_of_token t'))
  | _ -> failwith "Encountered unexpected end of token stream"

let rec parse (toks:token list) : (exp * token list) =
  if List.length toks = 0 then
    failwith "Unexpected end of token stream"
  else
       match peek toks with
       | TInt n  -> (EInt n, advance toks)
       | TFloat f -> (EFloat f, advance toks)
       | TNan -> (EFloat nan, advance toks)
       | TLParen -> let toks = consume TLParen toks in parse toks
       | TPlus -> 
           let toks = consume TPlus toks in
           let (e1, toks) =  parse toks in
           let (e2, toks) = parse toks in
           let toks = consume TRParen toks in
           (EAdd(e1, e2), toks)
       | TMinus ->
           let toks = consume TMinus toks in
           let (e1, toks) = parse toks in
           let (e2, toks) = parse toks in
           let toks = consume TRParen toks in
           (ESubtract(e1, e2), toks)
       | TMultiply ->
           let toks = consume TMultiply toks in
           let (e1, toks) = parse toks in
           let (e2, toks) = parse toks in
           let toks = consume TRParen toks in
           (EMultiply(e1, e2), toks)
       | TDivide ->
           let toks = consume TDivide toks in
           let (e1, toks) = parse toks in
           let (e2, toks) = parse toks in
           let toks = consume TRParen toks in
           (EDivide(e1, e2), toks)
       | TBoolean b -> (EBool b, advance toks)
       | TLessthanorequal ->
           let toks = consume TLessthanorequal toks in
           let (e1, toks) = parse toks in
           let (e2, toks) = parse toks in
           let toks = consume TRParen toks in
           (ELessthanorequal(e1, e2), toks)
       | TIf ->
           let toks = consume TIf toks in
           let (e1, toks) = parse toks in
           let (e2, toks) = parse toks in
           let (e3, toks) = parse toks in
           let toks = consume TRParen toks in
           (EIf(e1, e2, e3), toks)
       | t      ->   failwith (Printf.sprintf "Unexpected token found: %s" (string_of_token t))
