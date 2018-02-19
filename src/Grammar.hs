module Grammar where

data Pos = Pos { offset :: Int
               , line   :: Int
               , col    :: Int }
               deriving (Show, Eq)

data Exp =
      EAdd Pos Exp Exp
    | EMul Pos Exp Exp
    | ESub Pos Exp Exp
    | EDiv Pos Exp Exp
    | ELessEqThan Pos Exp Exp
    | EIf Pos Exp Exp Exp
    | EVal EValue
    deriving (Show, Eq)

data EValue = 
    EInt Pos Integer
  | EFloat Pos Float
  | EBool Pos Bool
  | ENaN Pos
  deriving (Show, Eq)