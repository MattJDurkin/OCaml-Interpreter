open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)


let rec parse_expr toks = 
  match (lookahead toks) with 
    | Some Tok_Let -> parse_let toks 
    | Some Tok_If -> parse_if toks 
    | Some Tok_Fun -> parse_fun toks 
    |_ -> parse_or toks
  
  and parse_let toks = match (lookahead_many toks 1) with
    | Some Tok_ID i -> let next1 = match_many toks [Tok_Let; Tok_ID i; Tok_Equal] in
      let (toks2, expr) = parse_expr next1 in 
        let next2 = match_token toks2 Tok_In in 
          let (toks3, expr2) = parse_expr next2 in
            (toks3, Let(i, false, expr, expr2))
    | Some Tok_Rec -> (match (lookahead_many toks 2) with
      | Some Tok_ID i -> let next1 = match_many toks [Tok_Let; Tok_Rec; Tok_ID i; Tok_Equal] in
        let (toks2, expr) = parse_expr next1 in 
          let next2 = match_token toks2 Tok_In in 
            let (toks3, expr2) = parse_expr next2 in
              (toks3, Let(i, true, expr, expr2))
      | _ -> raise(InvalidInputException("fail")) )  
    | _ -> raise(InvalidInputException("fail"))   

  and parse_fun toks = match (lookahead_many toks 1) with
    | Some Tok_ID i -> let next = match_many toks [Tok_Fun; Tok_ID i; Tok_Arrow] in
      let (toks2, expr) = parse_expr next in (toks2, Fun(i, expr))
    |_ -> raise(InvalidInputException("fail"))
  

  and parse_if toks = let next1 = match_token toks Tok_If in
    let (toks1, expr1) = parse_expr next1 in
      let next2 = match_token toks1 Tok_Then in let (toks2, expr2) = parse_expr next2 in
        let next3 = match_token toks2 Tok_Else in let (toks3, expr3) = parse_expr next3 in
          (toks3, If(expr1, expr2, expr3))

  and parse_or toks = let (toks2, expr) = parse_and toks in 
    match (lookahead toks2) with
    | Some Tok_Or -> let tokA = match_token toks2 Tok_Or in 
      let (toks3, expr2) = parse_or tokA in (toks3, Binop(Or, expr, expr2))
    | _ -> (toks2, expr)

  and parse_and toks = let (toks2, expr) = parse_equal toks in 
    match (lookahead toks2) with 
      | Some Tok_And -> let tokA = match_token toks2 Tok_And in
        let (toks3, expr2) = parse_and tokA in (toks3, Binop(And, expr, expr2))
      | _ -> (toks2, expr)

  and parse_equal toks = let (toks2, expr) = parse_relational toks in 
    match (lookahead toks2) with
      | Some Tok_Equal -> let tokA = match_token toks2 Tok_Equal in 
        let (toks3, expr2) = parse_equal tokA in (toks3, Binop(Equal, expr, expr2))
      | Some Tok_NotEqual -> let tokA = match_token toks2 Tok_NotEqual in 
        let (toks3, expr2) = parse_equal tokA in (toks3, Binop(NotEqual, expr, expr2))
      | _-> (toks2, expr)  

  and parse_relational toks = let (toks2, expr) = parse_additive toks in 
    match (lookahead toks2) with
      | Some Tok_Greater -> let tokA = match_token toks2 Tok_Greater in 
        let (toks3, expr2) = parse_equal tokA in (toks3, Binop(Greater, expr, expr2))
      | Some Tok_Less -> let tokA = match_token toks2 Tok_Less in 
        let (toks3, expr2) = parse_equal tokA in (toks3, Binop(Less, expr, expr2))
      | Some Tok_GreaterEqual -> let tokA = match_token toks2 Tok_GreaterEqual in 
        let (toks3, expr2) = parse_equal tokA in (toks3, Binop(GreaterEqual, expr, expr2))
      | Some Tok_LessEqual -> let tokA = match_token toks2 Tok_LessEqual in 
        let (toks3, expr2) = parse_equal tokA in (toks3, Binop(LessEqual, expr, expr2))
      | _-> (toks2, expr)

  and parse_additive toks = let (toks2, expr) = parse_multiplicative toks in 
    match (lookahead toks2) with
      | Some Tok_Add -> let tokA = match_token toks2 Tok_Add in 
        let (toks3, expr2) = parse_equal tokA in (toks3, Binop(Add, expr, expr2))
      | Some Tok_Sub -> let tokA = match_token toks2 Tok_Sub in 
        let (toks3, expr2) = parse_equal tokA in (toks3, Binop(Sub, expr, expr2))
      | _-> (toks2, expr)

  and parse_multiplicative toks = let (toks2, expr) = parse_concat toks in 
    match (lookahead toks2) with
      | Some Tok_Mult -> let tokA = match_token toks2 Tok_Mult in 
        let (toks3, expr2) = parse_equal tokA in (toks3, Binop(Mult, expr, expr2))
      | Some Tok_Div -> let tokA = match_token toks2 Tok_Div in 
        let (toks3, expr2) = parse_equal tokA in (toks3, Binop(Div, expr, expr2))
      | _-> (toks2, expr)

  and parse_concat toks = let (toks2, expr) = parse_unary toks in 
    match (lookahead toks2) with 
      | Some Tok_Concat -> let tokA = match_token toks2 Tok_Concat in
        let (toks3, expr2) = parse_and tokA in (toks3, Binop(Concat, expr, expr2))
      | _ -> (toks2, expr)

  and parse_unary toks = match (lookahead toks) with
    | Some Tok_Not -> let tokA = match_token toks Tok_Not in
      let (toks2, expr) = parse_unary tokA in (toks2, Not(expr))
    | _ -> parse_functioncall toks

  and parse_functioncall toks = let (toks2, expr) = parse_primaryexpr toks in
    match (lookahead toks2) with 
    | Some Tok_Int i ->(let (toks3, expr2) = parse_primaryexpr toks2 in (toks3, FunctionCall(expr, expr2)))
    | Some Tok_Bool i ->(let (toks3, expr2) = parse_primaryexpr toks2 in (toks3, FunctionCall(expr, expr2)))
    | Some Tok_String i ->(let (toks3, expr2) = parse_primaryexpr toks2 in (toks3, FunctionCall(expr, expr2)))
    | Some Tok_ID i ->(let (toks3, expr2) = parse_primaryexpr toks2 in (toks3, FunctionCall(expr, expr2)))
    | Some Tok_LParen -> (let (toks3, expr2) = parse_primaryexpr toks2 in (toks3, FunctionCall(expr, expr2)))
    | _ -> (toks2, expr)

  and parse_primaryexpr toks = match lookahead toks with
    | Some Tok_Int i -> let toks2 = match_token toks (Tok_Int i) in (toks2, Value (Int i))
    | Some Tok_Bool i -> let toks2 = match_token toks (Tok_Bool i) in (toks2, Value (Bool i))
    | Some Tok_ID i -> let toks2 = match_token toks (Tok_ID i) in (toks2, ID i)
    | Some Tok_String i -> let toks2 = match_token toks (Tok_String i) in (toks2, Value (String i))
    | Some Tok_LParen -> let toks2 = match_token toks (Tok_LParen) in 
      let (toks3, expr) = parse_expr toks2 in
        let toks4 = match_token toks3 Tok_RParen in (toks4, expr)
    | _ -> raise(InvalidInputException("Fail"));;

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match (lookahead toks) with 
    | Some Tok_Def -> mutop_def toks
    | Some Tok_DoubleSemi -> mutop_noop toks
    | _ -> mutop_expr toks

  and mutop_def toks = match (lookahead_many toks 1) with
    | Some Tok_ID i -> let next = match_many toks [Tok_Def; Tok_ID i; Tok_Equal] in
      let (toks2, expr) = parse_expr next in (toks2, Def(i, expr))
    |_ -> raise(InvalidInputException("fail"))

  and mutop_noop toks = (match_token toks Tok_DoubleSemi, NoOp)

  and mutop_expr toks = match parse_expr toks with 
    | ([Tok_DoubleSemi], t) -> ([], Expr t)
    | _ -> raise(InvalidInputException("fail"));;