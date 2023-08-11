open TokenTypes

let tokenize input = let re_bool = Str.regexp "\\(true\\|false\\)$" in let re_int = Str.regexp "[0-9]+\\|\\((-[0-9]+)\\)" in let re_string = Str.regexp "\"[^\"]*\"" in let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
let re_lparen = Str.regexp "(" in let re_rparen = Str.regexp ")" in let re_equal = Str.regexp "=" in let re_notequal = Str.regexp "<>" in let re_greater = Str.regexp ">" in let re_less = Str.regexp "<" in
let re_greaterequal = Str.regexp ">=" in let re_lessequal = Str.regexp "<=" in let re_or = Str.regexp "||" in let re_and = Str.regexp "&&" in let re_not = Str.regexp "not$" in let re_if = Str.regexp "if$" in
let re_then = Str.regexp "then$" in let re_else = Str.regexp "else$" in let re_add = Str.regexp "+" in let re_sub = Str.regexp "-" in let re_mult = Str.regexp "*" in let re_div = Str.regexp "/" in
let re_concat = Str.regexp "\\^" in let re_let = Str.regexp "let$" in let re_def = Str.regexp "def$" in let re_in = Str.regexp "in$" in let re_rec = Str.regexp "rec$" in let re_fun = Str.regexp "fun$" in let
re_arrow = Str.regexp "->" in let re_semi = Str.regexp ";;" in
        let rec helper pos = let length = String.length input in
                if pos >= length then []
                else if Str.string_match (Str.regexp " ") input pos then helper (pos + 1)
                else if Str.string_match re_string input pos then let str = Str.matched_string input in 
                Tok_String(String.sub str 1 ((String.length str)-2))::(helper (pos + String.length str))
                else if Str.string_match re_id input pos then let str = Str.matched_string input in
                        if Str.string_match re_or str 0 then Tok_Or::(helper (pos + 2))
                        else if Str.string_match re_and str 0 then Tok_And::(helper (pos + 2))
                        else if Str.string_match re_not str 0 then Tok_Not::(helper (pos + 3))
                        else if Str.string_match re_if str 0 then Tok_If::(helper (pos + 2))
                        else if Str.string_match re_then str 0 then Tok_Then::(helper (pos + 4))
                        else if Str.string_match re_else str 0 then Tok_Else::(helper (pos + 4))
                        else if Str.string_match re_let str 0 then Tok_Let::(helper (pos + 3))
                        else if Str.string_match re_def str 0 then Tok_Def::(helper (pos + 3))
                        else if Str.string_match re_in str 0 then Tok_In::(helper (pos + 2))
                        else if Str.string_match re_rec str 0 then Tok_Rec::(helper (pos + 3))
                        else if Str.string_match re_fun str 0 then Tok_Fun::(helper (pos + 3))
                        else if Str.string_match re_bool str 0 then
                        Tok_Bool(bool_of_string str)::(helper (pos + (String.length str)))
                        else Tok_ID(str)::(helper (pos + (String.length str)))
                else if Str.string_match re_int input pos then let str = Str.matched_string input in
                        if (String.sub str 0 1) = "(" then Tok_Int(int_of_string (String.sub str 1 
                        ((String.length str)-2)))::(helper (pos + String.length str))
                        else Tok_Int(int_of_string str)::(helper (pos + String.length str))
                else if Str.string_match re_arrow input pos then Tok_Arrow::(helper (pos + 2))
                else if Str.string_match re_lparen input pos then Tok_LParen::(helper (pos + 1))
                else if Str.string_match re_rparen input pos then Tok_RParen::(helper (pos + 1))
                else if Str.string_match re_equal input pos then Tok_Equal::(helper (pos + 1))
                else if Str.string_match re_notequal input pos then Tok_NotEqual::(helper (pos + 2))
                else if Str.string_match re_greaterequal input pos then Tok_GreaterEqual::(helper (pos + 2))
                else if Str.string_match re_lessequal input pos then Tok_LessEqual::(helper (pos + 2))
                else if Str.string_match re_greater input pos then Tok_Greater::(helper (pos + 1))
                else if Str.string_match re_less input pos then Tok_Less::(helper (pos + 1))
                else if Str.string_match re_add input pos then Tok_Add::(helper (pos + 1))
                else if Str.string_match re_sub input pos then Tok_Sub::(helper (pos + 1))
                else if Str.string_match re_mult input pos then Tok_Mult::(helper (pos + 1))
                else if Str.string_match re_div input pos then Tok_Div::(helper (pos + 1))
                else if Str.string_match re_concat input pos then Tok_Concat::(helper (pos + 1))
                else if Str.string_match re_semi input pos then Tok_DoubleSemi::(helper (pos + 2))
                else if Str.string_match re_or input pos then Tok_Or::(helper (pos + 2))
                else if Str.string_match re_and input pos then Tok_And::(helper (pos + 2))
                else raise (InvalidInputException "error")
in helper 0
