{
module Parser where

import Data.Char
import Lexer
import Grammar
}

%name parser
%tokentype { Token }

%left '+' '-'
%left '*' '/'
%right '->'

%token
  '('   { TLParen _ }
  ')'   { TRParen _ }
  '+'   { TPlus _   }
  '*'   { TMul _    }
  '-'   { TSub _    }
  '/'   { TDiv _    }
  if    { TIf _     }
  then  { TThen _   }
  else  { TElse _   }
  true  { TTrue _   }
  false { TFalse _  }
  nan   { TNaN _    }
  int   { TInt _ _ }
  float { TFloat _ _ }
  '<='  { TLessThan _ }
  let   { TLet _      }
  in    { TIn  _      }
  func  { TFunc _     }
  '='   { TAssign _   }
  var   { TVariable _ _ }
  '->'  { TArrow _ }

%%

Exp0 : if Exp0 then Exp0 else Exp0 { EIf (tokLoc $1) $2 $4 $6 }
     | let var = Exp1 in Exp1      { }
     | func var -> Exp1
     | Exp1 { $1 }

Exp1 : Exp1 '+' Exp1           { EAdd (tokLoc $2) $1 $3 }
    | Exp1 '-' Exp1            { ESub (tokLoc $2) $1 $3 }
    | Exp1 '/' Exp1            { EDiv (tokLoc $2) $1 $3 }
    | Exp1 '*' Exp1            { EMul (tokLoc $2) $1 $3 }
    | '(' Exp1 ')'             { $2                     }
    | int                      { buildValuedExp $1      }
    | float                    { buildValuedExp $1      }
    | true                     { EVal $ EBool (tokLoc $1) True }
    | false                    { EVal $ EBool (tokLoc $1) False  }
    | nan                      { EVal $ ENaN  (tokLoc $1)        }
    | Exp1 '<=' Exp1           { ELessEqThan (tokLoc $2) $1 $3 }

{

buildValuedExp :: Token -> Exp
buildValuedExp (TInt p i) = EVal $ EInt (alexPosnToPos p) i
buildValuedExp (TFloat p i) = EVal $ EFloat (alexPosnToPos p) i
buildValuedExp _ = error "Uncovered valued token in Parser.buildValuedExp"

happyError :: [Token] -> b
happyError _ = error ("Parse error\n")

}
