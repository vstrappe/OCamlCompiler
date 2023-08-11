open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(*type values = Int of int|Bool of bool|String of string*)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

let extend env x v = (x,v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with
        | Value v -> v
        | ID i -> ref_lookup env i
        | Not n -> let exp = eval_expr env n in (match exp with
                | Bool true -> Bool false
                | Bool false -> Bool true
                | _ -> raise (TypeError "Expected type bool"))
        | Binop (op, exp1, exp2) -> (match op with
                | Add -> (match (eval_expr env exp1), (eval_expr env exp2) with
                        | Int i1, Int i2 -> Int (i1 + i2)
                        | _ -> raise (TypeError "Expected type int"))
                | Sub -> (match (eval_expr env exp1), (eval_expr env exp2) with
                        | Int i1, Int i2 -> Int (i1 - i2)
                        | _ -> raise (TypeError "Expected type int"))
                | Mult -> (match (eval_expr env exp1), (eval_expr env exp2) with
                        | Int i1, Int i2 -> Int (i1 * i2)
                        | _ -> raise (TypeError "Expected type int"))
                | Div -> (match (eval_expr env exp1), (eval_expr env exp2) with
                        | Int i1, Int i2 -> if i2 = 0 then raise DivByZeroError else Int (i1 / i2)
                        | _ -> raise (TypeError "Expected type int"))
                | Greater -> (match (eval_expr env exp1), (eval_expr env exp2) with
                        | Int i1, Int i2 -> Bool (i1 > i2)
                        | _ -> raise (TypeError "Expected type int"))
                | Less -> (match (eval_expr env exp1), (eval_expr env exp2) with
                        | Int i1, Int i2 -> Bool (i1 < i2)
                        | _ -> raise (TypeError "Expected type int"))
                | GreaterEqual -> (match (eval_expr env exp1), (eval_expr env exp2) with
                        | Int i1, Int i2 -> Bool (i1 >= i2)
                        | _ -> raise (TypeError "Expected type int"))
                | LessEqual -> (match (eval_expr env exp1), (eval_expr env exp2) with
                        | Int i1, Int i2 -> Bool (i1 <= i2)
                        | _ -> raise (TypeError "Expected type int"))
                | Concat -> (match (eval_expr env exp1), (eval_expr env exp2) with
                        | String s1, String s2 -> String (s1 ^ s2)
                        | _ -> raise (TypeError "Expected type string"))
                | Equal -> (match (eval_expr env exp1), (eval_expr env exp2) with
                        | Int i1, Int i2 -> Bool (i1 = i2)
                        | String s1, String s2 -> Bool (s1 = s2)
                        | Bool b1, Bool b2 -> Bool (b1 = b2)
                        | _ -> raise (TypeError "Cannot compare types"))
                | NotEqual -> (match (eval_expr env exp1), (eval_expr env exp2) with
                        | Int i1, Int i2 -> Bool (i1 != i2)
                        | String s1, String s2 -> Bool (s1 != s2)
                        | Bool b1, Bool b2 -> Bool (b1 != b2)
                        | _ -> raise (TypeError "Cannot compare types"))
                | Or -> (match (eval_expr env exp1), (eval_expr env exp2) with
                        | Bool b1, Bool b2 -> Bool (b1 || b2)
                        | _ -> raise (TypeError "Expected type bool"))
                | And -> (match (eval_expr env exp1), (eval_expr env exp2) with
                        | Bool b1, Bool b2 -> Bool (b1 && b2)
                        | _ -> raise (TypeError "Expected type bool")))
        | If (exp1, exp2, exp3) -> (match (eval_expr env exp1) with
                | Bool true -> eval_expr env exp2
                | Bool false -> eval_expr env exp3
                | _ -> raise (TypeError "Expected type bool"))
        | Let (id, r, exp1, exp2) -> (match r with
                | false -> let v = eval_expr env exp1 in eval_expr (ref_extend env id v) exp2
                | true -> let n = (ref_extend_tmp env id) in let v = eval_expr n exp1 in let () = ref_update n id v in eval_expr n exp2)
        | Fun (id, exp) -> Closure(env, id, exp)
        | FunctionCall (f, v) -> let c = eval_expr env f in (match c with
                | Closure (a, x, e) -> let b = eval_expr env v in eval_expr (ref_extend a x b) e
                | _ -> raise (TypeError "Not a function"))


(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
        | Def (id, e) -> let n = (ref_extend_tmp env id) in let v = eval_expr n e in let () = ref_update n id v in (n, Some(v))
        | Expr e -> let exp = eval_expr env e in (env, Some(exp))
        | NoOp -> (env, None)
