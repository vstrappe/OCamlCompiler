open MicroCamlTypes
open Utils
open TokenTypes

let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h
  
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

let rec parse_expr toks = match lookahead toks with
        | Some Tok_If -> let (t, n) = parse_IfExpr toks in (t, n)
        | Some Tok_Let -> let (t, n) = parse_LetExpr toks in (t, n)
        | Some Tok_Fun -> let (t, n) = parse_FunctionExpr toks in (t, n)
        | _ -> let (t, n) = parse_OrExpr toks in (t, n)
and parse_LetExpr toks = let t = match_token toks Tok_Let in match lookahead t with
        | Some Tok_Rec -> let t' = match_token t Tok_Rec in (match lookahead t' with
                        | Some Tok_ID i -> let a = match_token t' (Tok_ID i) in let b = match_token a Tok_Equal in let (c, s) = 
                                           parse_expr b in let d = match_token c Tok_In in let (e, v) = parse_expr d 
                                           in (e, Let(i, true, s, v))
                        | _ -> raise (InvalidInputException "Happens after rec"))
        | Some Tok_ID i -> let a = match_token t (Tok_ID i) in let b = match_token a Tok_Equal in let (c, s) = parse_expr b in
                                      let d = match_token c Tok_In in let (e, v) = parse_expr d in (e, Let(i, false, s, v))
        | _ -> raise (InvalidInputException "happens after tok_id")
and parse_FunctionExpr toks = let t = match_token toks Tok_Fun in match lookahead t with
        | Some Tok_ID i -> let t' = match_many t [(Tok_ID i); Tok_Arrow] in let (t'', s) = parse_expr t' in (t'', Fun(i, s))
        | _ -> raise (InvalidInputException "Happens at fun")
and parse_IfExpr toks = match lookahead toks with
        | Some Tok_If -> let t = match_token toks Tok_If in let (t', s) = parse_expr t in let t'' = match_token t' Tok_Then in 
                        let (t''', c) = parse_expr t'' in let t'''' = match_token t''' Tok_Else in let (t''''', b) = parse_expr t'''' 
                        in (t''''', If(s, c, b))
        | _ -> raise (InvalidInputException "happens at if")
and parse_OrExpr toks = let (t, n) = parse_AndExpr toks in match lookahead t with
        | Some Tok_Or -> let t' = match_token t Tok_Or in let (t'', v) = parse_OrExpr t' in (t'', Binop(Or, n, v))
        | _ -> (t, n)
and parse_AndExpr toks = let (t, n) = parse_EqualityExpr toks in match lookahead t with
        | Some Tok_And -> let t' = match_token t Tok_And in let (t'', v) = parse_AndExpr t' in (t'', Binop(And, n, v))
        | _ -> (t, n)
and parse_EqualityExpr toks = let (t, n) = parse_RelationalExpr toks in match lookahead t with
        | Some Tok_Equal -> let t' = match_token t Tok_Equal in let (t'', s) = parse_EqualityExpr t' in (t'', Binop(Equal, n, s))
        | Some Tok_NotEqual -> let t' = match_token t Tok_NotEqual in let (t'', s) = parse_EqualityExpr t' in (t'', Binop(NotEqual, n, s))
        | _ -> (t, n)
and parse_RelationalExpr toks = let (t, n) = parse_AdditiveExpr toks in match lookahead t with
        | Some Tok_Greater -> let t' = match_token t Tok_Greater in let (t'', s) = parse_RelationalExpr t' in (t'', Binop(Greater, n, s))
        | Some Tok_Less -> let t' = match_token t Tok_Less in let (t'', s) = parse_RelationalExpr t' in (t'', Binop(Less, n, s))
        | Some Tok_GreaterEqual -> let t' = match_token t Tok_GreaterEqual in let (t'', s) = parse_RelationalExpr t' in (t'', Binop(GreaterEqual, n, s))
        | Some Tok_LessEqual -> let t' = match_token t Tok_LessEqual in let (t'', s) = parse_RelationalExpr t' in (t'', Binop(LessEqual, n, s))
        | _ -> (t, n)
and parse_AdditiveExpr toks = let (t, n) = parse_MultiplicativeExpr toks in match lookahead t with
        | Some Tok_Add -> let t' = match_token t Tok_Add in let (t'', s) = parse_AdditiveExpr t' in (t'', Binop(Add, n, s))
        | Some Tok_Sub -> let t' = match_token t Tok_Sub in let (t'', s) = parse_AdditiveExpr t' in (t'', Binop(Sub, n, s))
        | _ -> (t, n)
and parse_MultiplicativeExpr toks = let (t, n) = parse_ConcatExpr toks in match lookahead t with
        | Some Tok_Mult -> let t' = match_token t Tok_Mult in let (t'', s) = parse_MultiplicativeExpr t' in (t'', Binop(Mult, n, s))
        | Some Tok_Div -> let t' = match_token t Tok_Div in let (t'', s) = parse_MultiplicativeExpr t' in (t'', Binop(Div, n, s))
        | _ -> (t, n)
and parse_ConcatExpr toks = let (t, n) = parse_UnaryExpr toks in match lookahead t with
        | Some Tok_Concat -> let t' = match_token t Tok_Concat in let (t'', v) = parse_ConcatExpr t' in (t'', Binop(Concat, n, v))
        | _ -> (t, n)
and parse_UnaryExpr toks = match lookahead toks with
        | Some Tok_Not -> let t = match_token toks (Tok_Not) in let (t', s) = parse_UnaryExpr t in (t', Not(s))
        | _ -> let (t, s) = parse_FunctionCallExpr toks in (t, s)
and parse_FunctionCallExpr toks = let (t, n) = parse_PrimaryExpr toks in match lookahead t with
        | Some Tok_Int i -> let (t', s) = parse_PrimaryExpr t in (t', FunctionCall(n,s))
        | Some Tok_Bool b -> let (t', s) = parse_PrimaryExpr t in (t', FunctionCall(n,s))
        | Some Tok_String s -> let (t', s) = parse_PrimaryExpr t in (t', FunctionCall(n,s))
        | Some Tok_ID i -> let (t', s) = parse_PrimaryExpr t in (t', FunctionCall(n,s))
        | Some Tok_LParen -> let (t', s) = parse_PrimaryExpr t in (t', FunctionCall(n,s))
        | _ -> (t, n)
and parse_PrimaryExpr toks = match lookahead toks with
        | Some Tok_Int i -> let t = match_token toks (Tok_Int i) in (t, Value(Int i))
        | Some Tok_Bool b -> let t = match_token toks (Tok_Bool b) in (t, Value(Bool b))
        | Some Tok_String s -> let t = match_token toks (Tok_String s) in (t, Value(String s))
        | Some Tok_ID i -> let t = match_token toks (Tok_ID i) in (t, ID i)
        | Some Tok_LParen -> let t = match_token toks Tok_LParen in let (t', s) = parse_expr t in let t'' = match_token t' Tok_RParen in (t'', s)
        | _ -> raise (InvalidInputException "happens at the bottom")

let rec parse_mutop toks = match lookahead toks with
        | Some Tok_Def -> let (t, n) = parse_defmutop toks in (t,n)
        | Some Tok_DoubleSemi -> let t = match_token toks Tok_DoubleSemi in (t, NoOp)
        | _ -> let (t, n) = parse_exprmutop toks in (t,n)
and parse_defmutop toks = let t = match_token toks Tok_Def in match lookahead t with
        | Some Tok_ID i -> let a = match_many t [(Tok_ID i);Tok_Equal] in let (b, s) = parse_expr a in 
                           let c = match_token b Tok_DoubleSemi in (c, Def(i, s))
        | _ -> raise (InvalidInputException "error")
and parse_exprmutop toks = let (t, s) = parse_expr toks in let a = match_token t Tok_DoubleSemi in (a, Expr(s))
