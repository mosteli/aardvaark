module Eval where

{-# LANGUAGE BangPatterns #-}

import Grammar
import Control.Monad.Reader
import qualified Data.Map.Strict as Map

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
        var2@(EVar _ _) -> (binopToConstructor p op) var1 var2
        _ -> eval $ (binopToConstructor p op) (EVar p2 str) (eval e3)
    (e3, var1@(EVar _ _)) -> eval $ (binopToConstructor p op) (eval e3) var1
    (EVal _, _) -> binopToConstructor p op e1 (eval e2)
    (_, _) -> binopToConstructor p op (eval e1) e2

eval (EIf p e1 e2 e3) = case e1 of
    (EVal v1) -> case v1 of
        (EBool _ True) -> e2
        (EBool _ False) -> e3
        _ -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)
    (ELessEqThan _ r1 r2) -> case (r1, r2) of
        (EVal v1, EVal v2) -> case (v1, v2) of 
            ((EInt _ n1), (EInt _ n2)) -> if n1 <= n2 then e2 else e3
            ((EInt _ n1), (EFloat _ n2)) -> if (fromIntegral n1) <= n2 then e2 else e3
            ((EFloat _ n1), (EInt _ n2)) -> if n1 <= (fromIntegral n2) then e2 else e3
            ((EFloat _ n1), (EFloat _ n2)) -> if n1 <= n2 then e2 else e3
            _ -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)
    _ -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)

eval var@(EVar _ _) = var 

eval (ELet _ s (EVal v) e2 _) = subst v s e2 
eval (ELet p s e1 e2 t) = ELet p s (eval e1) e2 t

eval (EApp p e1 e2) = case (e1, e2) of
    (EVal v1, EVal v2) -> case v1 of
        (EFunc _ s funcExp _) -> subst v2 s funcExp
        fix@(EFix _ funcName varName funcExp _) -> let varReplaced = subst v2 varName funcExp in subst fix funcName varReplaced
        _ -> error $ (ppPos p) ++ " Function application operator applied where first paramater is not a function"
    (EVal _, var) -> case var of 
        (EVar _ _) -> error $ (ppPos p) ++ " Application of function to unresolved variable" 
        b@(EBinop _ _ _ _) -> EApp p e1 (eval b)
        _ -> error $ (ppPos p) ++ " Application of function to expression which did not resolve to a value"
    (_, _) -> EApp p (eval e1) (eval e2)

eval (EFst _ e1) = eval e1 
eval (ESnd _ e2) = eval e2
eval (EEmpty p e1) = 
    case e1 of 
        (EVal (ENil _ _)) -> EVal $ EBool p True
        _          -> EVal $ EBool p False 
eval (EHead p e) = 
    case e of 
        (EVal (ECons p e1 e2)) -> eval e1 
        (EVal (ENil _ _)) -> error $ (ppPos p) ++ " Application of head on empty list"
        _ -> error $ (ppPos p) ++ " Application of head on non-list type"
eval (ETail p e) = 
    case e of 
        (EVal (ECons p e1 e2)) -> eval e2 
        (EVal (ENil _ _)) -> error $ (ppPos p) ++ " Application of tail on empty list"
        _ -> error $ (ppPos p) ++ " Application of tail on non-list type"

eval e1 = e1

deepEval :: Exp -> Exp
deepEval v@(EVal _) = v
deepEval e = deepEval (eval e)

stepNEval :: Int -> Exp -> Exp
stepNEval _ v@(EVal _) = v
stepNEval 0 e = e
stepNEval n e = stepNEval (n-1) (eval e)

showExecutionSteps :: Exp -> IO ()
showExecutionSteps v@(EVal _) = putStrLn $ show v
showExecutionSteps e = do
    putStrLn $ show e 
    e <- helper (pure e)
    showExecutionSteps e
    where
        helper :: IO Exp -> IO Exp 
        helper ioe = do
            exp <- ioe
            putStrLn $ show exp
            return $ eval exp

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
subst val str (EVal (EFunc p funcStr funcE t))
    | str == funcStr = EVal (EFunc p funcStr (subst val str funcE) t)
    | otherwise = EVal (EFunc p funcStr (subst val str funcE) t)
subst val str e = recur e 
 where
    s = subst val str 
    recur :: Exp -> Exp
    recur (EBinop p  op e1 e2) = EBinop p op (s e1) (s e2)
    recur (ELessEqThan p e1 e2) = ELessEqThan p (s e1) (s e2)
    recur (EIf p e1 e2 e3) = EIf p (s e1) (s e2) (s e3)
    recur (ELet p str2 e1 e2 t) = ELet p str2 (s e1) (s e2) t
    recur func@(EVal (EFunc p funcStr funcE t))
        | str == funcStr = func -- avoid variable shadowing.
        | otherwise = EVal (EFunc p funcStr (s funcE) t) -- freely substite variables of name str in funcE
    recur (EApp p e1 e2) = EApp p (s e1) (s e2)
    recur (EFst p e) = EFst p (s e)
    recur (ESnd p e) = ESnd p (s e)
    recur (EEmpty p e) = EEmpty p (s e)
    recur (EHead p e) = EHead p (s e)
    recur (ETail p e) = ETail p (s e)
    recur e = e

evalFinal :: Exp -> IO ()
evalFinal (EVal v1) = case v1 of 
    (EInt _ i) -> putStrLn $ show i
    (EFloat _ i) -> putStrLn $ show i
    (ENaN _) -> putStrLn $ show "NaN"
    _ -> putStrLn $ show v1 
evalFinal e = putStrLn $ show e --error "Invalid argument"
