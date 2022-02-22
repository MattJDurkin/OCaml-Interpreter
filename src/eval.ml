open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with 
  | Value(Int i) -> Int i
  |  Value(Bool i) -> Bool i
  |  Value(String i) -> String i
  |  Value(Closure (env2, var, func)) -> Closure (env2, var, func) 
  |  ID i -> lookup env i 
  |  (Not i) -> let b = (eval_expr env ( i)) in (match b with 
    | Bool true -> Bool false 
    | Bool false -> Bool true
    | _ -> raise (TypeError ("Expected type bool")))
  |  Binop (o, i, j) -> let (x, y) = (eval_expr env ( i), eval_expr env ( j)) in (match o with 
    | Add -> (match (x,y) with 
      | (Int a, Int b)  -> Int (a + b)
      | _ -> raise (TypeError "Expected type int"))
    | Sub -> (match (x,y) with 
      | ( Int a,  Int b)  -> Int (a - b)
      | _ -> raise (TypeError "Expected type int"))
    | Mult -> (match (x,y) with 
      | ( Int a,  Int b)  -> Int (a * b)
      | _ -> raise (TypeError "Expected type int"))
    | Div -> (match (x,y) with 
      | ( Int a,  Int b)  ->  if b <> 0 then Int (a / b) else raise (DivByZeroError)
      | _ -> raise (TypeError "Expected type int"))
    | Greater -> (match (x, y) with
      | ( Int a,  Int b) -> if a > b then Bool true else Bool false
      | _ -> raise (TypeError "Expected type int"))
    | Less -> (match (x, y) with
      | ( Int a,  Int b) -> if a < b then Bool true else Bool false
      | _ -> raise (TypeError "Expected type int")  )
    | GreaterEqual -> (match (x, y) with
      | ( Int a,  Int b) -> if a >= b then Bool true else Bool false
      | _ -> raise (TypeError "Expected type int"))
    | LessEqual -> (match (x, y) with
      | ( Int a,  Int b) -> if a <= b then Bool true else Bool false
      | _ -> raise (TypeError "Expected type int"))
    | Concat -> (match (x, y) with
      | (String a, String b) -> String (a ^ b)
      | _ -> raise (TypeError "Expected type string"))
    | Equal -> (match (x, y) with 
      | (String a, String b) -> if a = b then Bool true else Bool false
      | (Int a, Int b) -> if a = b then Bool true else Bool false
      | (Bool a, Bool b) -> if a = b then Bool true else Bool false
      | _ -> raise (TypeError "Cannot compare types"))
    | NotEqual -> (match (x, y) with 
      | (String a, String b) -> if a <> b then Bool true else Bool false
      | (Int a, Int b) -> if a <> b then Bool true else Bool false
      | (Bool a, Bool b) -> if a <> b then Bool true else Bool false
      | _ -> raise (TypeError "Cannot compare types"))
    | Or -> (match (x, y) with 
      | (Bool a, Bool b) -> Bool (a || b)
      | _ -> raise (TypeError "Expected type bool"))
    | And -> (match (x, y) with 
      | (Bool a, Bool b) -> Bool (a && b)
      | _ -> raise (TypeError "Expected type bool")))
  |  If (g, t, f) -> let x = eval_expr env ( g) in (match x with 
    | Bool true -> eval_expr env ( t) 
    | Bool false -> eval_expr env ( f) 
    | _ -> raise (TypeError "Expected bool"))
  |  Let (v, b, init, body) -> 
    if b then
      let env2 = extend_tmp env v in
      let e = eval_expr env2 init in
      let () = update env2 v e in
      eval_expr env2 body
    else
      let e = eval_expr env init in
      let env2 = extend env v e in
      eval_expr env2 body
  | Fun (v, e) -> Closure (env, v, e)
  | FunctionCall (e1, e2) -> (match eval_expr env e1 with 
    | Closure (a, x, e) -> let v = eval_expr env e2 in 
      let env2 = extend a x v in eval_expr env2 e
    | _ -> raise (TypeError "Expected closure"));; 

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with 
  | Def (var, e) -> let env2 = extend_tmp env var in let x = eval_expr env2 e in let () = update env2 var x in (env2, Some x)
  | Expr e -> (env, Some (eval_expr env e))
  | NoOp -> ([], None)