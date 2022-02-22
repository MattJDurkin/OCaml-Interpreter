open TokenTypes

(* PASTE YOUR LEXER FROM P4A HERE *)

let tokenize input = 
  let length = String.length input in 
    let rec helper pos = 
        if pos >= length then []

        
        else if (Str.string_match(Str.regexp "[ \n\t]+") input pos) then
            helper (pos + 1)

        else if (Str.string_match (Str.regexp "(-[0-9]+)") input pos) then
        let matched_int = Str.matched_string input in 
            Tok_Int (int_of_string (String.sub matched_int 1 (String.length (matched_int) - 2))) :: (helper (pos + (String.length matched_int)))
        
        else if ((Str.string_match (Str.regexp "[0-9]+") input pos)) then
        let matched_int = Str.matched_string input in 
            Tok_Int (int_of_string matched_int) :: (helper (pos + (String.length matched_int)))        

        else if (Str.string_match (Str.regexp "(") input pos) then
            Tok_LParen :: (helper (pos + 1))
        else if (Str.string_match (Str.regexp ")") input pos) then
            Tok_RParen :: (helper (pos + 1))
        else if (Str.string_match (Str.regexp ">=") input pos) then
            Tok_GreaterEqual:: (helper (pos + 2))
        else if (Str.string_match (Str.regexp "<=") input pos) then
            Tok_LessEqual:: (helper (pos + 2))
        else if (Str.string_match (Str.regexp "<>") input pos) then
            Tok_NotEqual:: (helper (pos + 2))
        else if (Str.string_match (Str.regexp "=") input pos) then
            Tok_Equal:: (helper (pos + 2))
        else if (Str.string_match (Str.regexp ">") input pos) then
            Tok_Greater:: (helper (pos + 1))
        else if (Str.string_match (Str.regexp "<") input pos) then
            Tok_Less:: (helper (pos + 1))
        else if (Str.string_match (Str.regexp "||") input pos) then
            Tok_Or:: (helper (pos + 2))
        else if (Str.string_match (Str.regexp "&&") input pos) then
            Tok_And:: (helper (pos + 2))
        else if (Str.string_match (Str.regexp "not") input pos) then
            Tok_Not:: (helper (pos + 3))
        else if (Str.string_match (Str.regexp ";;") input pos) then
            Tok_DoubleSemi :: (helper (pos + 2))

        
        else if (Str.string_match (Str.regexp "\\^") input pos) then
            Tok_Concat :: (helper (pos + 1))
        else if (Str.string_match (Str.regexp "\\->") input pos) then
            Tok_Arrow:: (helper (pos + 2))
        else if (Str.string_match (Str.regexp "\\+") input pos) then
            Tok_Add:: (helper (pos + 1))
        else if (Str.string_match (Str.regexp "\\-") input pos) then
            Tok_Sub:: (helper (pos + 1))
        else if (Str.string_match (Str.regexp "\\*") input pos) then
            Tok_Mult:: (helper (pos + 1))
        else if (Str.string_match (Str.regexp "\\/") input pos) then
            Tok_Div:: (helper (pos + 1))

        
        else if (Str.string_match (Str.regexp "\"[^\"]*\"") input pos) then
            let matched_str = Str.matched_string input in 
                Tok_String (String.sub matched_str 1 (String.length (matched_str) - 2)) :: (helper (pos + (String.length matched_str)))
        
            
        else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos) then
        let matched_str = Str.matched_string input in 
            if(matched_str = "if") then Tok_If :: (helper (pos + 2))
            else if(matched_str = "else") then Tok_Else :: (helper (pos + 4))
            else if(matched_str = "then") then Tok_Then :: (helper (pos + 4))
            else if(matched_str = "rec") then Tok_Rec :: (helper (pos + 3))
            else if(matched_str = "let") then Tok_Let :: (helper (pos + 3))
            else if(matched_str = "def") then Tok_Def :: (helper (pos + 3))
            else if(matched_str = "in") then Tok_In :: (helper (pos + 2))
            else if(matched_str = "fun") then Tok_Fun :: (helper (pos + 3))
            else if(matched_str = "true") then Tok_Bool(true) :: (helper (pos + 4))
            else if(matched_str = "false") then Tok_Bool(false) :: (helper (pos + 5))
            else Tok_ID (matched_str) :: (helper (pos + (String.length matched_str)))
        
        
        
        else
          raise(InvalidInputException("lexer"))
      in helper 0