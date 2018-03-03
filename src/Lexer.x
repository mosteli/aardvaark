{
module Lexer (alexScanTokens, AlexPosn(..), tokLoc, Token(..), alexPosnToPos) where

import Grammar
}

%wrapper "posn"

$digit = 0-9
@decimal = $digit+
$lower = [a-z]
$alpha = [a-zA-Z]
@float = @decimal \. @decimal
@variable = $alpha+

tokens :-
$white+       ;
\(            { (\p s -> TLParen p)         }
\)            { (\p s -> TRParen p)         }
\+            { (\p s -> TPlus p)           }
\*            { (\p s -> TMul p)            }
\-            { (\p s -> TSub p)           }
\/            { (\p s -> TDiv p)           }
if            { (\p s -> TIf p)            }
then          { (\p s -> TThen p)          }
else          { (\p s -> TElse p)          }
true          { (\p s -> TTrue p)          }
false         { (\p s -> TFalse p)         }
\<\=          { (\p s -> TLessThan p)      }
NaN           { (\p s -> TNaN p)           }
@decimal      { (\p s -> TInt p (read s))    }
@float        { (\p s -> TFloat p (read s))  }
let           { (\p s -> TLet p)             }
in            { (\p s -> TIn p)              }
\=            { (\p s -> TAssign p)          }
func          { (\p s -> TFunc   p)          }
\-\>          { (\p s -> TArrow  p)          }
fix           { (\p s -> TFix p)        }
@variable     { (\p s -> TVariable p s) }
\~            { (\p s -> TApply p)      }

{

data Token =
    TSpaces AlexPosn
  | TLParen AlexPosn
  | TRParen AlexPosn
  | TPlus AlexPosn
  | TMul AlexPosn
  | TSub AlexPosn
  | TDiv AlexPosn
  | TIf AlexPosn
  | TThen AlexPosn
  | TElse AlexPosn
  | TTrue AlexPosn
  | TFalse AlexPosn
  | TLessThan AlexPosn
  | TNaN AlexPosn
  | TInt AlexPosn Integer
  | TFloat AlexPosn Float
  | TLet AlexPosn
  | TIn AlexPosn 
  | TAssign AlexPosn
  | TFunc AlexPosn
  | TArrow AlexPosn
  | TVariable AlexPosn String
  | TApply AlexPosn
  | TFix AlexPosn
  deriving (Eq)

-- Extracts AlexPosn from a given token
tokLoc :: Token -> Pos
tokLoc (TSpaces p) = alexPosnToPos p
tokLoc (TRParen p) = alexPosnToPos p
tokLoc (TLParen p) = alexPosnToPos p
tokLoc (TPlus p) = alexPosnToPos p
tokLoc (TMul p) = alexPosnToPos p
tokLoc (TSub p) = alexPosnToPos p
tokLoc (TDiv p) = alexPosnToPos p
tokLoc (TIf p) = alexPosnToPos p
tokLoc (TThen p) = alexPosnToPos p
tokLoc (TElse p) = alexPosnToPos p
tokLoc (TTrue p) = alexPosnToPos p
tokLoc (TFalse p) = alexPosnToPos p
tokLoc (TLessThan p) = alexPosnToPos p
tokLoc (TNaN p) = alexPosnToPos p
tokLoc (TInt p _) = alexPosnToPos p
tokLoc (TFloat p _) = alexPosnToPos p
tokLoc (TLet p) = alexPosnToPos p 
tokLoc (TIn p) = alexPosnToPos p 
tokLoc (TAssign p) = alexPosnToPos p 
tokLoc (TFunc p) = alexPosnToPos p 
tokLoc (TArrow p) = alexPosnToPos p 
tokLoc (TVariable p _) = alexPosnToPos p 
tokLoc (TApply p) = alexPosnToPos p
tokLoc (TFix p) = alexPosnToPos p

alexPosnToPos :: AlexPosn -> Pos
alexPosnToPos (AlexPn o l c) = Pos o l c

instance Show Token where
  show (TSpaces _) = " "
  show (TLParen _) = "("
  show (TRParen _) = ")"
  show (TPlus _)   = "+"
  show (TMul _)    = "*"
  show (TSub _)    = "-"
  show (TDiv _)    = "/"
  show (TIf _)     = "if"
  show (TThen _)   = "then"
  show (TElse _)   = "else"
  show (TTrue _)   = "true"
  show (TFalse _)  = "false"
  show (TLessThan _) = "<="
  show (TNaN _)      = "NaN"
  show (TInt _ n)    = show n
  show (TFloat _ n)  = show n
  show (TLet _) = "let"
  show (TIn _) = "in"
  show (TFunc _) = "func"
  show (TAssign _) = "="
  show (TArrow _) = "->"
  show (TVariable _ s) = "var " ++ s
  show (TApply _) = "~"
  show (TFix _) = "fix"

}