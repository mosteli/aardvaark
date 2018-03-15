{
module Parser where

import Data.Char
import Lexer
import Grammar
}

%name parser
%tokentype { Token }

%right '->'
%left '+' '-' '<='
%left '*' '/'
%left '!'
%left ';'
%left '<-'

%token
  '::'  { TTypeOf _ }
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
  '<'   { TLess _ }
  '>'   { TGreater _ }
  '<='  { TLessThan _ }
  let   { TLet _      }
  in    { TIn  _      }
  func  { TFunc _     }
  '='   { TAssign _   }
  fix   { TFix _   }
  var   { TVariable _ _ }
  '->'  { TArrow _ }
  '~'   { TApply _ }
  typeInt   { TIntType _ }
  typeFloat { TFloatType _ }
  typeBool  { TBoolType _ }
  unit      { TUnit _ }
  ','       { TComma _ }
  fst       { TFst _   }
  snd       { TSnd _   }
  empty     { TEmpty _ }
  head      { THead _ }
  tail      { TTail _ }
  ':'       { TCons _ }
  '[]'      { TEmptyList _ }
  '['       { TLBrack _ }
  ']'       { TRBrack _ }
  '-->'     { TLongArrow _ }
  ref       { TRef _ }
  '!'       { TBang _ }
  '<-'      { TAssignment _ }
  ';'       { TSemiColon _ }
  while     { TWhile _ }
  do        { TDo _ }
  end       { TEnd _ }
  record    { TRecord _ }
  recordEnd { TRecordEnd _ }
  with      { TWith _ }
  get       { TGet _ }
  '{'       { TLCurl _ }
  '}'       { TRCurl _ }

%%

Exp0 : if Exp0 then Exp0 else Exp0 { EIf (tokLoc $1) $2 $4 $6 }
     | let var '::' Type '=' Exp0 in Exp0    
       { ELet 
         (tokLoc $1) 
         (varString $2) 
         $6 
         $8
         $4 }
     | func    '(' var '::' Type ')' '::' Type '-->' Exp0 
       { EVal 
          (EFunc 
            (tokLoc $1) 
            (varString $3) 
            $10
            (YApp $5 $8)) }
     | fix var '(' var '::' Type ')' '::' Type '-->' Exp0
        { EVal 
          (EFix
            (tokLoc $1)
            (varString $2)
            (varString $4)
            $11
            (YApp $6 $9)) }
     | '(' Exp0 ',' Exp0 ')'       
       { EVal 
          (EPair 
            (tokLoc $1)
            $2
            $4) }
     | while Exp0 do Exp0 end      { EWhile (tokLoc $1) $2 $4 $2 $4 }
     | fst Exp0                    { EFst (tokLoc $1) $2   }
     | snd Exp0                    { ESnd (tokLoc $1) $2   }
     | head Exp0                   { EHead (tokLoc $1) $2  }
     | tail Exp0                   { ETail (tokLoc $1) $2  }
     | empty Exp0                  { EEmpty (tokLoc $1) $2 }
     | ref Exp0                    { ERef (tokLoc $1) $2   }
     | Exp0 '<-' Exp0              { EAssignment (tokLoc $2) $1 $3 }
     | Exp0 ';' Exp0               { EStatement (tokLoc $2) $1 $3  }  
     | List                        { $1 }
     | Record                      { $1 }
     | get var Exp0                { EGetField (tokLoc $1) (varString $2) $3 }
     | '(' Exp0 ')'                { $2 }
     | Exp1                        { $1 }

Type : typeInt               { YInt   }
     | typeFloat             { YFloat }
     | typeBool              { YBool  }
     | '<' Type '>'          { YRef $2  }
     | Type '->' Type        { YApp $1 $3  }
     | '(' Type ',' Type ')' { YPair $2 $4 }
     | '[' Type ']'          { YList $2    }
     | '{' RecordType '}'    { $2 }

RecordType : var '::' Type ',' RecordType 
              { YParsedRecord (varString $1) $3 $5 }
           | var '::' Type                
              { YParsedRecord (varString $1) $3 YParsedRecordEnd }

List : '[]' '::' Type      { EVal (ENil (tokLoc $1) $3)     }
     | Exp0 ':' Exp0       { EVal (ECons (tokLoc $2) $1 $3) } 

Record : record var '::' Type '=' Exp0 with Exp0 
       { EVal 
          (ERecordField 
            (tokLoc $1) 
            (varString $2) 
            $4 
            $6
            $8) }
        | recordEnd { EVal (ERecordEnd (tokLoc $1)) }

Exp1 : Exp1 '+' Exp1           { EBinop (tokLoc $2) Add $1 $3    }
    | Exp1 '-' Exp1            { EBinop (tokLoc $2) Sub $1 $3    }
    | Exp1 '/' Exp1            { EBinop (tokLoc $2) Div $1 $3    }
    | Exp1 '*' Exp1            { EBinop (tokLoc $2) Mul $1 $3    }
    | Exp1 '~' Exp1            { EApp   (tokLoc $2) $1 $3        }
    | '(' Exp1 ')'             { $2                              }
    | int                      { buildValuedExp $1               }
    | float                    { buildValuedExp $1               }
    | true                     { EVal $ EBool (tokLoc $1) True   }
    | false                    { EVal $ EBool (tokLoc $1) False  }
    | nan                      { EVal $ ENaN  (tokLoc $1)        }
    | Exp1 '<=' Exp1           { ELessEqThan (tokLoc $2) $1 $3   }
    | var                      { EVar (tokLoc $1) (varString $1) }
    | unit                     { EVal (EUnit (tokLoc $1))        }
    |'!' Exp1                  { EBang (tokLoc $1) $2            }

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
