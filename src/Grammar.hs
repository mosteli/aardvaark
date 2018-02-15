module Grammar where

data Token =
    TSpaces
  | TLParen
  | TRParen
  | TPlus
  | TMul
  | TSub
  | TDiv
  | TIf
  | TThen
  | TElse
  | TTrue
  | TFalse
  | TLessThan
  | TNaN
  | TInt Integer
  | TFloat Float
  deriving (Eq,Show)

data Exp =
      EAdd Exp Exp
    | EMul Exp Exp
    | ESub Exp Exp
    | EDiv Exp Exp
    | EInt Integer
    | EFloat Float
    | ELessEqThan Exp Exp
    | EIf Exp Exp Exp
    | EBool Bool
    | ENaN
    deriving (Show, Eq)
