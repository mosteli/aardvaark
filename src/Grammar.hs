module Grammar where

data Pos = Pos { offset :: Int
               , line   :: Int
               , col    :: Int }
               deriving (Show, Eq)

data Exp =
      EBinop Pos EOptype Exp Exp
    | ELessEqThan Pos Exp Exp
    | EIf Pos Exp Exp Exp
    | ELet Pos Exp Exp
    | EVar Pos String
    | EVal EValue
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
  | EFunc Pos Exp
  deriving (Show, Eq)