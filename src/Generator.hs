module Generator
where

import Grammar

emitTextA :: Assign -> String
emitTextA (Assign a b) = emitText b ++ emitLn ("Mov [" ++ a ++ "], eax")

emitText :: Expression -> String
emitText expr = case expr of    
    Num a   -> emitLn ("MOV eax, " ++ (show a))
    Var a   -> emitLn ("MOV eax, [" ++ a ++ "]")
    Add a b -> emitText a ++ pushEax ++ emitText b ++ add 
    Sub a b -> emitText a ++ pushEax ++ emitText b ++ sub
    Mul a b -> emitText a ++ pushEax ++ emitText b ++ mul 
    Div a b -> emitText a ++ pushEax ++ emitText b ++ divide

emitDataA :: Assign -> String
emitDataA (Assign a b) = emitData b 

emitData :: Expression -> String
emitData expr = case expr of 
        Add a b -> emitData a ++ emitData b 
        Sub a b -> emitData a ++ emitData b 
        Mul a b -> emitData a ++ emitData b 
        Div a b -> emitData a ++ emitData b 
        Var a   -> emitLn  (a ++ "\tdd\t0") 
        _ -> ""

emitBssA :: Assign -> String
emitBssA (Assign a b) = emitLn (a ++ "\tresd\t1") ++ emitBss b 

emitBss :: Expression -> String
emitBss expr = case expr of
        Add a b -> emitBss a ++ emitBss b
        Sub a b -> emitBss a ++ emitBss b
        Mul a b -> emitBss a ++ emitBss b 
        Div a b -> emitBss a ++ emitBss b 
        _ -> ""

emitLn s = "\t" ++ s ++ "\n"

emit :: Assign -> String
emit a = "section .data\n"    ++ emitDataA a
         ++ "section .bss\n"  ++ emitBssA  a
         ++ "section .text\n" ++ emitTextA a

--Had to change from this to ^ to fit type sigs; why is book wrong??  
{-
emit :: Expression -> String
emit expr = "section .data\n" ++ emitDataA expr
         ++ "section .bss\n"  ++ emitBssA  expr 
         ++ "section .text\n" ++ emitTextA expr
-}
popEbx = emitLn "POP ebx"
popEax = emitLn "POP eax"
pushEax = emitLn "PUSH eax"
add = popEbx ++ emitLn "ADD eax, ebx"
sub = popEbx ++ emitLn "SUB eax, ebx" ++ emitLn "NEG eax"

mul = popEbx ++ emitLn "MUL ebx"
divide = emitLn "MOV ebx, eax" ++ popEax ++ emitLn "MOV edx, 0" ++ emitLn "DIV ebx"