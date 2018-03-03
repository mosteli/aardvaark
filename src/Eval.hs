module Eval where

{-# LANGUAGE BangPatterns #-}

import Grammar

eval :: Exp -> Exp
eval (EBinop p op e1 e2) = case (e1, e2) of 
    (EVal v1, EVal v2) -> case (v1, v2) of
        ((EInt _ n1), r2) -> case r2 of
            (EInt _ n2) -> EVal $ (EInt p) $ (if op == Div then div else (binopIntFunc op)) n1 n2
            (EFloat _ n2) -> EVal $ (EFloat p) $ (binopFracFunc op) (fromIntegral n1) n2
            _           -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)
        ((EFloat _ n1), (EFloat _ n2)) -> EVal $ (EFloat p) $ (binopFracFunc op) n1 n2
        ((EFloat _ n1), r2) -> case r2 of
            (EInt _ n2) -> EVal $ (EFloat p) $ (binopFracFunc op) n1 (fromIntegral n2)
            (ENaN _) -> EVal $ ENaN p
            _        -> error $ "Type mismatch at column " ++ show (col p) ++ " , line " ++ show (line p)
        ((ENaN _), r2) -> case r2 of
            (ENaN _) -> EVal $ ENaN p
            (EFloat _ _) -> EVal $ ENaN p
        (_, _) -> error $ (show op) ++ " invalid types error at column " ++ show (col p) ++ " , line " ++ show (line p)
    (var1@(EVar p2 str), e3) -> case e3 of
        var2@(EVar p3 str2) -> (binopToConstructor p op) var1 var2
        _ -> eval $ (binopToConstructor p op) (EVar p2 str) (eval e3)
    (e3, var1@(EVar p2 str)) -> eval $ (binopToConstructor p op) (eval e3) var1
    (_, _) -> eval $ (binopToConstructor p op) (eval e1) (eval e2)

eval (EIf p e1 e2 e3) = case e1 of
    (EVal v1) -> case v1 of
        (EBool _ True) -> eval e2
        (EBool _ False) -> eval e3
        _ -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)
    (ELessEqThan p r1 r2) -> case (r1, r2) of
        (EVal v1, EVal v2) -> case (v1, v2) of 
            ((EInt _ n1), (EInt _ n2)) -> if n1 < n2 then eval e2 else eval e3
            ((EInt _ n1), (EFloat _ n2)) -> if (fromIntegral n1) < n2 then eval e2 else eval e3
            ((EFloat _ n1), (EInt _ n2)) -> if n1 < (fromIntegral n2) then eval e2 else eval e3
            ((EFloat _ n1), (EFloat _ n2)) -> if n1 < n2 then eval e2 else eval e3
            _ -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)
    _ -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)

eval var@(EVar p s) = var 

eval (ELet _ s (EVal v) e2) = eval $ subst v s e2 
eval (ELet p s e1 e2) = eval (ELet p s (eval e1) e2)

eval (EApp p e1 e2) = case (e1, e2) of
    (EVal v1, EVal v2) -> case v1 of
        (EFunc _ s funcExp) -> eval $ subst v2 s funcExp
        fix@(EFix _ funcName varName funcExp) -> let varReplaced = subst v2 varName funcExp in subst fix funcName varReplaced
        _ -> error $ (ppPos p) ++ " Function application operator applied where first paramater is not a function"
    (EVal _, var) -> case var of 
        (EVar _ _) -> error $ (ppPos p) ++ " Application of function to unresolved variable" 
        _ -> error $ (ppPos p) ++ " Application of function to expression which did not resolve to a value"
    (_, _) -> eval $ EApp p (eval e1) (eval e2)

eval e1 = e1

binopIntFunc :: Integral a => EOptype -> a -> a -> a
binopIntFunc Add = (+)
binopIntFunc Sub = (-)
binopIntFunc Mul = (*)
binopIntFunc Div = div

binopFracFunc :: Fractional a => EOptype -> a -> a -> a
binopFracFunc Add = (+)
binopFracFunc Sub = (-)
binopFracFunc Mul = (*)
binopFracFunc Div = (/)

binopToConstructor :: Pos -> EOptype -> Exp -> Exp -> Exp
binopToConstructor p Add = EBinop p Add
binopToConstructor p Sub = EBinop p Sub
binopToConstructor p Mul = EBinop p Mul
binopToConstructor p Div = EBinop p Div

-- 
subst :: EValue -- Value to substitute in  
    -> String -- Name of variable to substitute with value
    -> Exp -- Expression to do substituting in
    -> Exp -- Resulting expression with pertinent variables with name String substituted for EValue
subst val str var@(EVar _ s) = if s == str then EVal val else var
subst val str (EVal (EFunc p funcStr funcE))
    | str == funcStr = EVal (EFunc p funcStr (subst val str funcE))
    | otherwise = EVal (EFunc p funcStr (subst val str funcE))
subst val str e = recur e 
 where
    s = subst val str 
    recur :: Exp -> Exp
    recur (EBinop p  op e1 e2) = EBinop p op (s e1) (s e2)
    recur (ELessEqThan p e1 e2) = ELessEqThan p (s e1) (s e2)
    recur (EIf p e1 e2 e3) = EIf p (s e1) (s e2) (s e3)
    recur (ELet p str2 e1 e2) = ELet p str2 (s e1) (s e2)
    recur func@(EVal (EFunc p funcStr funcE))
        | str == funcStr = func -- avoid variable shadowing.
        | otherwise = EVal (EFunc p funcStr (s funcE)) -- freely substite variables of name str in funcE
    recur (EApp p e1 e2) = EApp p (s e1) (s e2)
    recur e = e 

-- substituting with a function is a bit differ
-- A function provides a name for which it wants you to substitute a value

evalFinal :: Exp -> IO ()
evalFinal (EVal v1) = case v1 of 
    (EInt _ i) -> putStrLn $ show i
    (EFloat _ i) -> putStrLn $ show i
    (ENaN _) -> putStrLn $ show "NaN"
    (EFunc _ s exp) -> putStrLn $ show v1
evalFinal e = putStrLn $ show e --error "Invalid argument"
