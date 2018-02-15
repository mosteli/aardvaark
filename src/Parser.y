{
module Parser where

import Data.Char
import Grammar
}

%name parser
%tokentype { Token }

%left '+' '-'
%left '*' '/'

%token
  '('   { TLParen }
  ')'   { TRParen }
  '+'   { TPlus   }
  '*'   { TMul    }
  '-'   { TSub    }
  '/'   { TDiv    }
  if    { TIf     }
  then  { TThen  }
  else  { TElse  }
  true  { TTrue  }
  false { TFalse }
  nan   { TNaN   }
  int   { TInt $$ }
  float { TFloat $$ }
  '<='  { TLessThan }

%%

Exp : Exp '+' Exp              { EAdd   $1 $3 }
    | Exp '-' Exp              { ESub   $1 $3 }
    | Exp '/' Exp              { EDiv   $1 $3 }
    | Exp '*' Exp              { EMul   $1 $3 }
    | '(' Exp ')'              { $2           }
    | int                      { EInt   $1    }
    | float                    { EFloat $1    }
    | if Exp then Exp else Exp { EIf $2 $4 $6 }
    | true                     { EBool True   }
    | false                    { EBool False  }
    | nan                      { ENaN          }
    | Exp '<=' Exp             { ELessEqThan $1 $3 }
{

happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

}
