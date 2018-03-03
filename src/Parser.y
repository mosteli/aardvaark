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
%left '~'

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
  fix   { TFix _   }
  var   { TVariable _ _ }
  '->'  { TArrow _ }
  '~'   { TApply _ }

%%

Exp0 : if Exp0 then Exp0 else Exp0 { EIf (tokLoc $1) $2 $4 $6 }
     | let var '=' Exp0 in Exp0    { ELet (tokLoc $1) (varString $2) $4 $6 }
     | func var '->' Exp0          { EVal (EFunc (tokLoc $1) (varString $2) $4) }
     | fix var var '->' Exp0       { EVal (EFix (tokLoc $1) (varString $2) (varString $3) $5) }
     | Exp0 '~' Exp0               { EApp (tokLoc $2) $1 $3 }
     | '(' Exp0 ')' { $2 }
     | Exp1 { $1 }

ExpVar : var                      { EVar (tokLoc $1) (varString $1) }

Exp1 : Exp1 '+' Exp1           { EBinop (tokLoc $2) Add $1 $3 }
    | Exp1 '-' Exp1            { EBinop (tokLoc $2) Sub $1 $3 }
    | Exp1 '/' Exp1            { EBinop (tokLoc $2) Div $1 $3 }
    | Exp1 '*' Exp1            { EBinop (tokLoc $2) Mul $1 $3 }
    | '(' Exp1 ')'             { $2                     }
    | int                      { buildValuedExp $1      }
    | float                    { buildValuedExp $1      }
    | true                     { EVal $ EBool (tokLoc $1) True  }
    | false                    { EVal $ EBool (tokLoc $1) False }
    | nan                      { EVal $ ENaN  (tokLoc $1)       }
    | Exp1 '<=' Exp1           { ELessEqThan (tokLoc $2) $1 $3  }
    | ExpVar { $1 }

{

varString :: Token -> String 
varString (TVariable _ s) = s
varString _ = error "varString used on non-TVariable token"

buildValuedExp :: Token -> Exp
buildValuedExp (TInt p i) = EVal $ EInt (alexPosnToPos p) i
buildValuedExp (TFloat p i) = EVal $ EFloat (alexPosnToPos p) i
buildValuedExp _ = error "Uncovered valued token in Parser.buildValuedExp"

happyError :: [Token] -> b
happyError _ = error ("Parse error\n")

}
