module Grammar where

import qualified Data.Map.Strict as Map

data Pos = Pos { offset :: Int
               , line   :: Int
               , col    :: Int }
               deriving (Eq)

instance Show Pos where
  show _ = ""

ppPos :: Pos -> String
ppPos (Pos _ l c) = "Line:Column of " ++ (show l) ++ ":" ++ (show c)

data Exp =
      EBinop Pos EOptype Exp Exp
    | ELessEqThan Pos Exp Exp
    | EIf  Pos Exp Exp Exp
    | ELet Pos String Exp Exp YType
    | EVal EValue
    | EVar Pos String
    | EApp Pos Exp Exp
    | EFst Pos Exp 
    | ESnd Pos Exp 
    | EHead Pos Exp  
    | ETail Pos Exp  
    | EEmpty Pos Exp 
    deriving (Show, Eq)

data EOptype = 
    Add 
  | Sub 
  | Mul 
  | Div 
  deriving (Show, Eq)

data EValue = 
    EInt Pos Integer
  | EFloat Pos Float
  | EBool Pos Bool
  | ENaN Pos
  | EFunc Pos String Exp YType
  | EFix Pos String String Exp YType 
  | EPair Pos Exp Exp
  | ENil Pos YType 
  | ECons Pos Exp Exp
  | EUnit Pos 
  deriving (Eq)

data YType = 
    YInt
  | YFloat 
  | YBool  
  | YUnit  
  | YApp YType YType
  | YPair YType YType
  | YList YType
  deriving (Eq, Show)

newtype TypeEnv = TypeEnv (Map.Map String YType)

-- instance Show Exp where
--   show (EBinop p op e1 e2) = (show op) ++ (show e1) ++ (show e2)
--   show (ELessEqThan p e1 e2) = " <= " ++ (show e1) ++ (show e2)
--   show (EIf _ e1 e2 e3) = "if " ++ (show e1) ++ " then " ++ (show e2)
--     ++ " else " ++ (show e3)
--   show (ELet _ s e1 e2) = "let " ++ s ++ " = " ++ (show e1) ++ " in"
--     ++ (show e2)
--   show (EVal v1) = show v1
--   show (EVar _ s) = "var " ++ s 
--   show (EApp _ e1 e2) = (show e1) ++ "~" ++ (show e2)

instance Show EValue where
  show (EInt _ i) = show i 
  show (EFloat _ f) = show f 
  show (EBool _ b) = "E" ++ show b 
  show (ENaN _) = "EaN"
  show (EFunc _ s e t) = "EFunc " ++ (show t) ++ s ++ " -> " ++ show e
  show (EFix _ s1 s2 e t) = "EFix " ++ (show t) ++ s1 ++ " " ++ s2 ++ " -> " ++ show e
  show (EPair _ e1 e2) = "EPair " ++ (show e1) ++ " " ++ (show e2)
  show (EUnit _) = "EUnit"
  show (ECons _ e1 e2) = "ECons " ++ (show e1) ++ " " ++ (show e2)
  show (ENil _ typ) = "ENil " ++ (show typ)