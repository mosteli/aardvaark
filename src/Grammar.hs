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
    | ERef Pos Exp
    | EBang Pos Exp 
    | EAssignment Pos Exp Exp 
    | EStatement Pos Exp Exp 
    | EWhile Pos Exp Exp Exp Exp
    | EGetField Pos String Exp
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
  | ERecordField Pos String YType Exp Exp 
  | ERecordEnd Pos
  | EUnit Pos 
  | EEvalUnit 
  | EPtr Int
  deriving (Eq)

data YType = 
    YInt
  | YFloat 
  | YBool  
  | YUnit  
  | YApp YType YType
  | YPair YType YType
  | YList YType
  | YRef YType
  | YParsedRecord String YType YType -- rightmost YType should be a YParsedRecord  
  | YParsedRecordEnd 
  | YRecordField [(String, YType)]
  deriving (Eq, Show)

newtype TypeEnv = TypeEnv (Map.Map String YType)

type EvalEnv = [(Int, EValue)]

instance Show EValue where
  show (EInt _ i) = show i 
  show (EFloat _ f) = show f 
  show (EBool _ b) = "E" ++ show b 
  show (ENaN _) = "EaN"
  show (EFunc _ s e t) = "EFunc " ++ (show t) ++ s ++ " -> " ++ show e
  show (EFix _ s1 s2 e t) = "EFix " ++ (show t) ++ s1 ++ " " ++ s2 ++ " -> " ++ show e
  show (EPair _ e1 e2) = "EPair " ++ (show e1) ++ " " ++ (show e2)
  show (EUnit _) = "EUnit"
  show EEvalUnit = "EEvalUnit"
  show (ECons _ e1 e2) = "ECons " ++ (show e1) ++ " " ++ (show e2)
  show (ENil _ typ) = "ENil " ++ (show typ)
  show (EPtr i) = "EPtr " ++ (show i)
  show (ERecordField _ str typ e1 rest) = "record " ++ (show str) ++ " :: " 
    ++ (show typ) ++ " = " ++ (show e1) ++ " " ++ (show rest)
  show (ERecordEnd _) = "recordEnd" 