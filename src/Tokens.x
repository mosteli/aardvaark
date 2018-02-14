{
module Tokens (alexScanTokens) where
}

%wrapper "basic"

$digit = 0-9			-- digits
@decimal = $digit+
$alpha = [a-zA-Z]		-- alphabetic characters
@float = @decimal \. @decimal

tokens :-

  $white+       ;
  \(            { \s -> TLParen }
  \)            { \s -> TRParen }
  \+            { \s -> TPlus   }
  \*            { \s -> TMul    }
  \-            { \s -> TSub    }
  \/            { \s -> TDiv    }
  if            { \s -> TIf     }
  true          { \s -> TTrue   }
  false         { \s -> TFalse  }
  \<\=          { \s -> TLessThan }
  NaN           { \s -> NaN     }
  $digit+       { \s -> TInt (read s) }
  @float        { \s -> TFloat (read s) }
{
-- Each right-hand side has type :: String -> Token

-- The token type:
data Token =
    TSpaces
  | TLParen
  | TRParen
  | TPlus
  | TMul
  | TSub
  | TDiv
  | TIf
  | TTrue
  | TFalse
  | TLessThan
  | NaN
  | TInt Integer
  | TFloat Float
  deriving (Eq,Show)
