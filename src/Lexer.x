{
module Lexer (alexScanTokens) where

import Grammar
}

%wrapper "basic"

$digit = 0-9
@decimal = $digit+
$alpha = [a-zA-Z]
@float = @decimal \. @decimal

tokens :-
$white+       ;
\(            { \s -> TLParen    }
\)            { \s -> TRParen    }
\+            { \s -> TPlus      }
\*            { \s -> TMul       }
\-            { \s -> TSub       }
\/            { \s -> TDiv       }
if            { \s -> TIf        }
then          { \s -> TThen      }
else          { \s -> TElse      }
true          { \s -> TTrue       }
false         { \s -> TFalse      }
\<\=          { \s -> TLessThan   }
NaN           { \s -> TNaN         }
@decimal      { \s -> TInt (read s) }
@float        { \s -> TFloat (read s) }
