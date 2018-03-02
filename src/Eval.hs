module Eval where

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
        (_, _) -> error $ "Type mismatch at column " ++ show (col p) ++ " , line " ++ show (line p)
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

evalFinal :: Exp -> IO ()
evalFinal (EVal v1) = case v1 of 
    (EInt _ i) -> putStrLn $ show i
    (EFloat _ i) -> putStrLn $ show i
    (ENaN _) -> putStrLn $ show "NaN"
evalFinal _ = error "Invalid argument"
