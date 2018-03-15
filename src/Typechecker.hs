module Typechecker where 

import Grammar
import Control.Monad.Reader
import qualified Data.Map.Strict as Map

typecheck' :: Exp -> YType
typecheck' e = runReader (typecheck e) (TypeEnv Map.empty)

typecheck :: Exp -> Reader TypeEnv YType
typecheck binop@(EBinop _ _ e1 e2) = do
    t1 <- typecheck e1
    t2 <- typecheck e2
    if t1 == t2 
        then return t1 
        else error $ pTypeError binop
typecheck (EVal v) = case v of 
    (EInt _ _) -> return YInt
    (EFloat _ _) -> return YFloat
    (EBool _ _) -> return YBool
    (ENaN _) -> return YFloat
    efunc@(EFunc _ str e1 typ) ->
        case typ of 
            (YApp paramType returnType) -> do
                t1 <- local (insertType str paramType) $ typecheck e1
                (TypeEnv tv) <- ask 
                if t1 == returnType 
                    then return typ 
                    else error $ (show tv) ++ (show t1) ++ (show returnType) ++ (pTypeError $ EVal efunc)
    efix@(EFix _ funcStr varStr e1 typ) -> 
        case typ of 
            (YApp paramType returnType) -> do 
                t1 <- local (insertList [(funcStr, typ), (varStr, paramType)]) $ typecheck e1
                if t1 == returnType 
                    then return typ 
                    else error $ pTypeError $ EVal efix
    (EPair _ e1 e2) -> do 
        t1 <- typecheck e1 
        t2 <- typecheck e2 
        return $ YPair t1 t2 
    (ENil _ t) -> return t 
    econs@(ECons _ e1 e2) -> do 
        t1 <- typecheck e1
        t2 <- typecheck e2
        case t2 of 
            (YList typ) -> 
                if t1 == typ
                    then return t2 
                    else error $ pTypeError $ EVal econs
            _ -> error $ pTypeError $ EVal econs
    erec@(ERecordField _ str typ e1 rest) -> do 
        t1 <- typecheck e1 
        t2 <- typecheck rest 
        if t1 == typ
            then return $ YParsedRecord str typ t2 
            else error $ pTypeError $ EVal erec 
    (ERecordEnd _) -> return $ YParsedRecordEnd
typecheck evar@(EVar _ varName) = do
    (TypeEnv env) <- ask
    case Map.lookup varName env of 
        Just varType -> return varType
        Nothing -> error $ pTypeError evar
typecheck lte@(ELessEqThan _ e1 e2) = do
    t1 <- typecheck e1 
    t2 <- typecheck e2
    if t1 == t2 && (t1 == YInt || t1 == YFloat)
        then return YBool
        else error $ pTypeError lte 
typecheck eif@(EIf _ e1 e2 e3) = do 
    t1 <- typecheck e1 
    t2 <- typecheck e2 
    t3 <- typecheck e3 
    (TypeEnv te) <- ask
    if not (t1 == YBool)
        then error $ (show te) ++ pTypeError eif
        else if t2 == t3
            then return t2 
            else error $ pTypeError eif 
typecheck eapp@(EApp _ e1 e2) = do 
    t1 <- typecheck e1 
    t2 <- typecheck e2
    (TypeEnv m) <- ask
    case t1 of 
        (YApp paramType returnType) -> if t2 == paramType
            then return returnType 
            else error $ pTypeError eapp
        _ -> error $ show m 
typecheck elet@(ELet _ str e1 e2 typ) = do 
    t1 <- typecheck e1
    t2 <- local (insertType str t1) $ typecheck e2  
    if t1 == typ -- Let just stores the type of e1 for us to check. We don't constrain t2's type.
        then return t2
        else error $ pExtendedTypeError typ t1 elet 
typecheck efst@(EFst _ e) = do
  t1 <- typecheck e 
  case t1 of 
    (YPair left _) -> return left
    _ -> error $ pTypeError efst
typecheck esnd@(ESnd _ e) = do
  t1 <- typecheck e 
  case t1 of 
    (YPair _ right) -> return right 
    _ -> error $ pTypeError esnd
typecheck ehead@(EHead _ e) = do 
  t1 <- typecheck e 
  case t1 of 
    (YList t) -> return t1 
    _ -> error $ pTypeError ehead 
typecheck etail@(ETail _ e) = do
  t1 <- typecheck e 
  case t1 of 
    (YList t) -> return t1 
    _ -> error $ pTypeError etail
typecheck eempty@(EEmpty _ e) = do
  t1 <- typecheck e 
  case t1 of 
    (YList t) -> return YBool 
    _ -> error $ pTypeError eempty  
typecheck (ERef _ e) = do 
    t1 <- typecheck e
    return $ YRef t1
typecheck ebang@(EBang _ e) = do 
    t1 <- typecheck e
    case t1 of 
        (YRef enclosedType) -> return enclosedType
        _ -> error $ pTypeError ebang  
typecheck eassign@(EAssignment _ e1 e2) = do
    t1 <- typecheck e1
    typeOfExpressionBeingAssigned <- typecheck e2 
    case t1 of 
        (YRef enclosedType) -> if enclosedType == typeOfExpressionBeingAssigned
            then return YUnit
            else error $ pTypeError eassign
typecheck estmt@(EStatement _ e1 e2) = do
    t1 <- typecheck e1 
    t2 <- typecheck e2 
    case t1 of 
        YUnit -> return t2 
        _ -> error $ pTypeError estmt
typecheck ewhile@(EWhile _ e1 e2 e3 e4) = do
    t1 <- typecheck e1 
    t2 <- typecheck e2 
    case (t1, t2) of 
        (YBool, YUnit) -> return YUnit 
        (_, _) -> error $ pTypeError ewhile
typecheck egetfield@(EGetField p str e) = do 
    t1 <- typecheck e 
    case t1 of 
        YRecordField ls -> 
            case lookup str ls of 
                Just fieldType -> return fieldType 
                Nothing -> error $ "getField called on record without given field"
        recT@(YParsedRecord fieldName fieldType rest) -> 
            if recContainsVar str recT
                then return $ recGetVar str recT 
                else error $ pTypeError egetfield 
        YParsedRecordEnd -> error $ "getField called on record without given field"
        _ -> error $ pTypeError egetfield 

recContainsVar :: String -> YType -> Bool 
recContainsVar var recType = 
    case recType of 
        YParsedRecord fieldName fieldType rest -> 
            if var == fieldName
                then True 
                else recContainsVar var rest 
        YParsedRecordEnd -> False 

recGetVar :: String -> YType -> YType 
recGetVar var (YParsedRecord fieldName fieldType rest)
    | fieldName == var = fieldType 
    | otherwise        = recGetVar var rest 

insertType :: String -> YType -> TypeEnv -> TypeEnv
insertType str typ (TypeEnv m) = TypeEnv (Map.insert str typ m)

insertList :: [(String, YType)] -> TypeEnv -> TypeEnv 
insertList ls (TypeEnv env) = TypeEnv $ helper ls env 
    where 
        helper :: [(String, YType)] 
               -> Map.Map String YType 
               -> Map.Map String YType 
        helper [] m = m
        helper ((str, typ):ls) m = 
            helper ls $ Map.insert str typ m

pTypeError :: Exp -> String
pTypeError e = "Type mismatch on " ++ show e

pExtendedTypeError :: YType -> YType -> Exp -> String 
pExtendedTypeError expected actual e = pTypeError e ++ 
    "\nExpected: " ++ (show expected) ++ 
    "\nActual: " ++ (show actual)