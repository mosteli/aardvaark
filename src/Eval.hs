module Eval where

import Grammar

eval :: Exp -> Exp
eval (EAdd p e1 e2) = case (e1, e2) of
    (EVal v1, EVal v2) -> case (v1, v2) of
        ((EInt _ n1), r2) -> case r2 of
            (EInt _ n2) -> EVal $ (EInt p) $ n1 + n2
            (EFloat _ n2) -> EVal $ (EFloat p) $ (fromIntegral n1) + n2
            _           -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)
        ((EFloat _ n1), (EFloat _ n2)) -> EVal $ (EFloat p) $ n1 + n2
        ((EFloat _ n1), r2) -> case r2 of
            (EInt _ n2) -> EVal $ (EFloat p) $ n1 + (fromIntegral n2)
            (ENaN _) -> EVal $ ENaN p
            _        -> error $ "Type mismatch at column " ++ show (col p) ++ " , line " ++ show (line p)
        ((ENaN _), r2) -> case r2 of
            (ENaN _) -> EVal $ ENaN p
            (EFloat _ _) -> EVal $ ENaN p
        (_, _) -> error $ "Type mismatch at column " ++ show (col p) ++ " , line " ++ show (line p)
    (_, _) -> eval $ EAdd p (eval e1) (eval e2)

eval (EMul p e1 e2) = case (e1, e2) of
    (EVal v1, EVal v2) -> case (v1, v2) of
        ((EInt _ n1), r2) -> case r2 of
            (EInt _ n2) -> EVal $ (EInt p) $ n1 * n2
            (EFloat _ n2) -> EVal $ (EFloat p) $ (fromIntegral n1) * n2
            _           -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)
        ((EFloat _ n1), (EFloat _ n2)) -> EVal $ (EFloat p) $ n1 * n2
        ((EFloat _ n1), r2) -> case r2 of
            (EInt _ n2) -> EVal $ (EFloat p) $ n1 * (fromIntegral n2)
            (ENaN _) -> EVal $ ENaN p
            _        -> error $ "Type mismatch at column " ++ show (col p) ++ " , line " ++ show (line p)
        ((ENaN _), r2) -> case r2 of
            (ENaN _) -> EVal $ ENaN p
            (EFloat _ _) -> EVal $ ENaN p
        (_, _) -> error $ "Type mismatch at column " ++ show (col p) ++ " , line " ++ show (line p)
    (_, _) -> eval $ EMul p (eval e1) (eval e2)

eval (ESub p e1 e2) = case (e1, e2) of
    (EVal v1, EVal v2) -> case (v1, v2) of
        ((EInt _ n1), r2) -> case r2 of
            (EInt _ n2) -> EVal $ (EInt p) $ n1 - n2
            (EFloat _ n2) -> EVal $ (EFloat p) $ (fromIntegral n1) - n2
            _           -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)
        ((EFloat _ n1), (EFloat _ n2)) -> EVal $ (EFloat p) $ n1 - n2
        ((EFloat _ n1), r2) -> case r2 of
            (EInt _ n2) -> EVal $ (EFloat p) $ n1 - (fromIntegral n2)
            (ENaN _) -> EVal $ ENaN p
            _        -> error $ "Type mismatch at column " ++ show (col p) ++ " , line " ++ show (line p)
        ((ENaN _), r2) -> case r2 of
            (ENaN _) -> EVal $ ENaN p
            (EFloat _ _) -> EVal $ ENaN p
        (_, _) -> error $ "Type mismatch at column " ++ show (col p) ++ " , line " ++ show (line p)
    (_, _) -> eval $ ESub p (eval e1) (eval e2)

eval (EDiv p e1 e2) = case (e1, e2) of
    (EVal v1, EVal v2) -> case (v1, v2) of
        ((EInt _ n1), r2) -> case r2 of
            (EInt _ 0) -> error $ "Division by zero at" ++ show (col p) ++ ", line " ++ show (line p)
            (EInt _ n2) -> EVal $ (EInt p) $ n1 `div` n2
            (EFloat _ n2) -> EVal $ (EFloat p) $ (fromIntegral n1) / n2
            _           -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)
        ((EFloat _ n1), (EFloat _ n2)) -> EVal $ (EFloat p) $ n1 / n2
        ((EFloat _ n1), r2) -> case r2 of
            (EFloat _ 0)  -> error $ "Division by zero at" ++ show (col p) ++ ", line " ++ show (line p)
            (EInt _ n2) -> EVal $ (EFloat p) $ n1 / (fromIntegral n2)
            (ENaN _) -> EVal $ ENaN p
            _        -> error $ "Type mismatch at column " ++ show (col p) ++ " , line " ++ show (line p)
        ((ENaN _), r2) -> case r2 of
            (ENaN _) -> EVal $ ENaN p
            (EFloat _ _) -> EVal $ ENaN p
    (_, _) -> eval $ EDiv p (eval e1) (eval e2)

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

evalFinal :: Exp -> IO ()
evalFinal (EVal v1) = case v1 of 
    (EInt _ i) -> putStrLn $ show i
    (EFloat _ i) -> putStrLn $ show i
    (ENaN _) -> putStrLn $ show "NaN"
evalFinal _ = error "Invalid argument"
