module Eval where

{-# LANGUAGE BangPatterns #-}

import Grammar
import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import Data.List

eval :: EvalEnv -> Exp -> (EvalEnv, Exp)
eval env (EBinop p op e1 e2) = case (e1, e2) of 
    (EVal v1, EVal v2) -> case (v1, v2) of
        ((EInt _ n1), r2) -> case r2 of
            (EInt _ n2) -> let v = EVal $ (EInt p) $ (if op == Div then div else (binopIntFunc op)) n1 n2 in (env, v)
            (EFloat _ n2) -> let v = EVal $ (EFloat p) $ (binopFracFunc op) (fromIntegral n1) n2 in (env, v)
            _           -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)
        ((EFloat _ n1), (EFloat _ n2)) -> let v = EVal $ (EFloat p) $ (binopFracFunc op) n1 n2 in (env, v)
        ((EFloat _ n1), r2) -> case r2 of
            (EInt _ n2) -> let v = EVal $ (EFloat p) $ (binopFracFunc op) n1 (fromIntegral n2) in (env, v)
            (ENaN _) -> (env, EVal $ ENaN p)
            _        -> error $ "Type mismatch at column " ++ show (col p) ++ " , line " ++ show (line p)
        ((ENaN _), r2) -> case r2 of
            (ENaN _) -> (env, EVal $ ENaN p)
            (EFloat _ _) -> (env, EVal $ ENaN p)
        (_, _) -> error $ (show op) ++ " invalid types error at column " ++ show (col p) ++ " , line " ++ show (line p)
    (var1@(EVar p2 str), e3) -> case e3 of
        var2@(EVar _ _) -> (env, (binopToConstructor p op) var1 var2)
        _ -> case eval env e3 of 
            (env2, newE) -> eval env2 $ (binopToConstructor p op) (EVar p2 str) newE
    (e3, var1@(EVar _ _)) -> case eval env e3 of 
        (env2, newE3) -> eval env2 $ (binopToConstructor p op) newE3 var1
    (EVal _, _) -> case eval env e2 of 
        (env2, newE2) -> (env2, binopToConstructor p op e1 newE2)
    (_, _) -> case eval env e1 of 
        (env2, newE1) -> (env2, binopToConstructor p op newE1 e2)

eval env (EIf p e1 e2 e3) = case e1 of
    (EVal v1) -> case v1 of
        (EBool _ True) -> (env, e2)
        (EBool _ False) -> (env, e3)
        _ -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)
    elte@(ELessEqThan _ r1 r2) -> eval env elte

eval env (ELessEqThan p e1 e2) = case (e1, e2) of
    (EVal v1, EVal v2) -> case (v1, v2) of 
        ((EInt _ n1), (EInt _ n2)) -> if n1 <= n2 then envTrue else envFalse
        ((EInt _ n1), (EFloat _ n2)) -> if (fromIntegral n1) <= n2 then envTrue else envFalse
        ((EFloat _ n1), (EInt _ n2)) -> if n1 <= (fromIntegral n2) then envTrue else envFalse
        ((EFloat _ n1), (EFloat _ n2)) -> if n1 <= n2 then envTrue else envFalse
        _ -> error $ "Type mismatch at column " ++ show (col p) ++ ", line " ++ show (line p)
    (EVal v1, _) -> case eval env e2 of 
        (env2, newE2) -> (env2, ELessEqThan p e1 newE2)
    (_, _) -> case eval env e1 of
        (env2, newE1) -> (env2, ELessEqThan p newE1 e2)
    where 
        envTrue  = (env, (EVal (EBool p True)))
        envFalse = (env, (EVal (EBool p False)))


eval env var@(EVar _ _) = (env, var) 

eval env (ELet _ s (EVal v) e2 _) = (env, subst v s e2)
eval env (ELet p s e1 e2 t) = case eval env e1 of
    (env2, exp2) -> (env2, ELet p s exp2 e2 t)

eval env (EApp p e1 e2) = case (e1, e2) of
    (EVal v1, EVal v2) -> case v1 of
        (EFunc _ s funcExp _) -> (env, subst v2 s funcExp) -- seems good 
        fix@(EFix _ funcName varName funcExp _) -> let varReplaced = subst v2 varName funcExp 
                                                   in (env, subst fix funcName varReplaced)
        _ -> error $ (ppPos p) ++ " Function application operator applied where first paramater is not a function"
    (EVal _, var) -> case var of 
        (EVar _ _) -> error $ (ppPos p) ++ " Application of function to unresolved variable" 
        b@(EBinop _ _ _ _) -> case eval env b of 
            (env2, exp2) -> (env2, EApp p e1 exp2)
        _ -> error $ (ppPos p) ++ " Application of function to expression which did not resolve to a value"
    (_, _) -> case eval env e1 of
        (env2, exp2) -> case eval env e2 of
            (env3, exp3) -> (env2 ++ env3, EApp p exp2 exp3)

eval env (EFst _ (EVal (EPair _ e1 _))) = (env, e1) 
eval env (ESnd _ (EVal (EPair _ _ e2))) = (env, e2)
eval env (EEmpty p e1) = 
    case e1 of 
        (EVal (ENil _ _)) -> (env, EVal $ EBool p True)
        _          -> (env, EVal $ EBool p False)
eval env (EHead p e) = 
    case e of 
        (EVal (ECons p e1 e2)) -> (env, e1)
        (EVal (ENil _ _)) -> error $ (ppPos p) ++ " Application of head on empty list"
        _ -> error $ (ppPos p) ++ " Application of head on non-list type"
eval env (ETail p e) = 
    case e of 
        (EVal (ECons p e1 e2)) -> (env, e2)
        (EVal (ENil _ _)) -> error $ (ppPos p) ++ " Application of tail on empty list"
        _ -> error $ (ppPos p) ++ " Application of tail on non-list type"
eval env (ERef p e) = 
    case e of 
        (EVal v) -> let ptr = length env in (env ++ [(ptr, v)], (EVal (EPtr ptr)))
eval env (EAssignment p e1 e2) = 
    case (e1, e2) of 
        ((EVal (EPtr n)), (EVal newValue)) -> 
            case lookup n env of 
                Just oldValue -> let oldPair = (n, oldValue) in 
                    let l = delete oldPair env in 
                    ((n, newValue):l, EVal $ EEvalUnit)
                Nothing -> error $ (ppPos p) ++ " Reassignment to reference missing from environement"
        (EVal (EPtr n), _) -> case eval env e2 of 
            (env2, newE2) -> (env2, EAssignment p e1 newE2)
        (_, _) ->  case eval env e1 of 
            (env2, newE1) -> (env2, EAssignment p newE1 e2)
eval env (EBang p e) = 
    case e of 
        (EVal (EPtr n)) -> case lookup n env of 
            Just val -> (env, EVal $ val)
            _        -> error $ (ppPos p) ++ " Dereference of variable not found in environment"
eval env (EStatement p e1 e2) = 
    let (env2, newE1) = eval env e1 in
    case e1 of 
        (EAssignment _ _ _) -> 
            case newE1 of 
                (EVal EEvalUnit) -> eval env2 e2
                _ -> error $ (ppPos p) ++ " Assignment expression returned non-unit expression"
        (EWhile _ _ _ _ _) -> 
            case newE1 of 
                (EVal EEvalUnit) -> eval env2 e2
                _ -> (env2, EStatement p newE1 e2)
        _ -> error $ (ppPos p) ++ " Non-assignment/while expression used before statement operator"                
eval env (EWhile p cachedBool cachedBody bool body) = 
    case bool of -- e1 is just a holder of the original expr which should be of type bool
        (EVal (EBool _ True))  ->
            case eval env body of 
                (env2, (EVal v)) -> (env2, EWhile p cachedBool cachedBody cachedBool cachedBody)
                (env2, newBody) -> (env2, EWhile p cachedBool cachedBody bool newBody)
        (EVal (EBool _ False)) ->
            case eval env body of 
                (env2, _) -> (env2, (EVal EEvalUnit))            
        _ -> case eval env bool of 
            (env2, newBool) -> (env2, EWhile p cachedBool cachedBody newBool body)

eval env e1 = eval env e1
-- 
subst :: EValue -- Value to substitute in  
    -> String -- Name of variable to substitute with value
    -> Exp -- Expression to do substituting in
    -> Exp -- Resulting expression with pertinent variables with name String substituted for EValue
subst val str var@(EVar _ s) = if s == str then EVal val else var
subst val str (EVal (EFunc p funcStr funcE t))
    | str == funcStr = error $ "here" -- EVal (EFunc p funcStr (subst val str funcE) t)
    | otherwise      = error $ "here" -- EVal (EFunc p funcStr (subst val str funcE) t)
subst val str e = recur e 
 where
    s = subst val str 
    recur :: Exp -> Exp
    recur (EBinop p  op e1 e2) = EBinop p op (s e1) (s e2)
    recur (ELessEqThan p e1 e2) = ELessEqThan p (s e1) (s e2)
    recur (EIf p e1 e2 e3) = error $ "here " --EIf p (s e1) (s e2) (s e3)
    recur (ELet p str2 e1 e2 t) = ELet p str2 (s e1) (s e2) t
    recur func@(EVal (EFunc p funcStr funcE t))
        | str == funcStr = func -- avoid variable shadowing.
        | otherwise = EVal (EFunc p funcStr (s funcE) t) -- freely substite variables of name str in funcE
    recur (EApp p e1 e2)  = EApp p (s e1) e2
    recur (EFst p e1)     = EFst p (s e1)
    recur (ESnd p e1)     = ESnd p (s e1)
    recur (EEmpty p e1)   = EEmpty p (s e1)
    recur (EHead p e1)    = EHead p (s e1)
    recur (ETail p e1)    = ETail p (s e1)
    recur (EBang p e1)    = EBang p (s e1)
    recur (ERef p e1)     = ERef p (s e1)
    recur (EAssignment p e1 e2) = EAssignment p (s e1) (s e2)
    recur (EStatement p e1 e2) = EStatement p (s e1) (s e2)
    recur (EWhile p e1 e2 e3 e4) = EWhile p (s e1) (s e2) (s e3) (s e4)
    recur (EVal v) = error $ "recur on value: " ++ (show v)
    recur e = e -- Keeps uninvolved expressions the way they are

evalFinal :: Exp -> IO ()
evalFinal (EVal v1) = case v1 of 
    (EInt _ i) -> putStrLn $ show i
    (EFloat _ i) -> putStrLn $ show i
    (ENaN _) -> putStrLn $ show "NaN"
    _ -> putStrLn $ show v1 
evalFinal e = putStrLn $ show e --error "Invalid argument"

deepEval :: EvalEnv -> Exp -> (EvalEnv, Exp)
deepEval env v@(EVal _) = (env, v)
deepEval env e = case eval env e of 
    (env2, e2) -> deepEval env2 e2

deepEval' :: EvalEnv -> Exp -> Exp 
deepEval' env e = snd $ deepEval env e 

stepNEval :: EvalEnv -> Int -> Exp -> (EvalEnv, Exp)
stepNEval env _ v@(EVal _) = (env, v)
stepNEval env 0 e = (env, e)
stepNEval env n e = case eval env e of
    (env2, e2) -> stepNEval env2 (n-1) e2

showExecutionSteps :: EvalEnv -> Exp -> IO ()
showExecutionSteps env v@(EVal _) = putStrLn $ (show v) ++ "\nEnv: " ++ (show env)
showExecutionSteps env e = do
    (env2, exp2) <- helper $ pure (env, e)
    showExecutionSteps env2 exp2
    where
        helper :: IO (EvalEnv, Exp) -> IO (EvalEnv, Exp) 
        helper ioExpr = do
            (env, exp) <- ioExpr
            putStrLn $ (show exp) ++ "\nEnv: " ++ (show env)
            case eval env exp of
                (env2, exp2) -> return (env2, exp2)

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