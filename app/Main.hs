module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Char

main :: IO ()
main = greetBasic =<< execParser opts
    where
        opts = info (basic <**> helper)
            ( fullDesc
            <> progDesc "Compile nothing at all!"
            <> header "aardvark - a fuzzy animal compiler" )

greetBasic :: Basic -> IO ()
greetBasic (Basic p) = evalFinal $ eval $ parsePair $ lx p

--parses the program to a set of intermediate tokens
lexProgram :: String -> [Token]
lexProgram [] = []
lexProgram (l:ls)
    | l == '('  = SymToken LParen : lexProgram ls
    | l == ')'  = SymToken RParen : lexProgram ls
    | l == '+'  = SymToken Plus : lexProgram ls
    | l == '/'  = SymToken Div : lexProgram ls
    | l == '*'  = SymToken Mul : lexProgram ls
    | l == '-'  = SymToken Sub : lexProgram ls
    | l == ' '  = SymToken Spc : lexProgram ls
lexProgram (l:m:ls)
    | l == 'i' && m == 'f' = SymToken If : lexProgram ls
    | l == '<' && m == '=' = SymToken LessT : lexProgram ls
lexProgram (l:m:n:ls)
    | l == 'N' && m == 'a' && n == 'N' = SymToken NotNumber : lexProgram ls
lexProgram (l:m:n:o:ls)
    | l == 't' && m == 'r' && n == 'u' && o == 'e' = SymToken Tru : lexProgram ls
lexProgram (l:m:n:o:p:ls)
    | l == 'f' && m == 'a' && n == 'l' && o == 's' && p == 'e' = SymToken Fal : lexProgram ls
lexProgram ls 
    | isFloat ls = FloatToken (read float :: Float) : lexProgram (drop (length float) ls)
    | otherwise  = IntToken (read int') : lexProgram (drop (length int') ls) 
    where 
        float = "0" ++ takeWhile (liftA2 (||) (== '.') isDigit) ls 
        int'  = takeWhile isDigit ls

isFloat :: String -> Bool
isFloat (l:ls')
    | isDigit l = isFloat ls'
    | l == '.'  = True
    | otherwise = False
isFloat _ = False

lexNumTokens :: [Token] -> [Token]
lexNumTokens ls = foldr f [] ls
    where 
        f :: Token -> [Token] -> [Token]
        f (NumToken n) ((IntToken i):ns) = (IntToken (i * 10 + numberToInt n)):ns
        f (NumToken n) ns = (IntToken $ numberToInt n) : ns
        f sym ns = sym:ns

removeWhiteSpace :: [Token] -> [Token]
removeWhiteSpace ls = foldr f [] ls
    where
        f :: Token -> [Token] -> [Token]
        f (SymToken Spc) ns = ns
        f n ns = n:ns

lx :: String -> [Token]
lx = removeWhiteSpace . lexNumTokens . lexProgram

floatLexTest :: String
floatLexTest = "(+ 1.1234123 1.12341243)"

test :: [Token]
test = [SymToken LParen, SymToken Plus, IntToken 1, IntToken 2, SymToken RParen]

test2 :: [Token]
test2 = [SymToken LParen, SymToken Plus, SymToken LParen, SymToken Plus, IntToken 1, IntToken 2, SymToken RParen, IntToken 2, SymToken RParen]

test3 :: [Token]
test3 = 
    [SymToken LParen, SymToken Plus, 
        SymToken LParen, SymToken Plus, IntToken 1, IntToken 2, SymToken RParen, 
        SymToken LParen, SymToken Plus, IntToken 1, IntToken 2, SymToken RParen,
    SymToken RParen]

test4 :: [Token]
test4 =
    [SymToken LParen, SymToken Sub,
        SymToken LParen, SymToken Sub, IntToken 10, IntToken 2, SymToken RParen, 
        SymToken LParen, SymToken Mul, IntToken 100, IntToken 2, SymToken RParen,
    SymToken RParen]

test5 :: [Token]
test5 = 
    [SymToken LParen, 
        SymToken If, 
            SymToken LParen, SymToken LessT, 
                SymToken LParen, SymToken Plus, IntToken 2, IntToken 3, SymToken RParen,
                SymToken LParen, SymToken Plus, IntToken 2, IntToken 3, SymToken RParen,
            SymToken RParen,
            IntToken 2, 
            IntToken 3,
    SymToken RParen]

test6 :: [Token]
test6 = 
    [SymToken LParen, SymToken If, 
        SymToken LParen, SymToken LessT, 
            SymToken LParen, SymToken Plus, IntToken 2, IntToken 3, SymToken RParen,
            SymToken LParen, SymToken Plus, IntToken 2, IntToken 3, SymToken RParen,
        SymToken RParen,
        SymToken LParen, SymToken Plus, IntToken 1, IntToken 2, SymToken RParen, 
        SymToken LParen, SymToken Plus, IntToken 1, IntToken 2, SymToken RParen,
    SymToken RParen]

parsePair :: [Token] -> Exp
parsePair (SymToken LParen:SymToken Mul:ls)  = EMul (extractParam ls) (rightParam 1 ls)
parsePair (SymToken LParen:SymToken Plus:ls) = EAdd (extractParam ls) (rightParam 1 ls)
parsePair (SymToken LParen:SymToken Sub:ls)  = ESub (extractParam ls) (rightParam 1 ls)
parsePair (SymToken LParen:SymToken Div:ls)  = EDiv (extractParam ls) (rightParam 1 ls)
parsePair (SymToken LParen:SymToken If:ls)
    | not ((removeLeftParam 2 (SymToken LParen:SymToken If:ls)) == []) = EIf (extractParam ls) (rightParam 1 ls) (rightParam 2 ls)
    | otherwise = EIf (extractParam ls) (rightParam 1 ls) (rightParam 1 ls)
parsePair (SymToken LParen:SymToken LessT:ls) = ELessEqThan (extractParam ls) (rightParam 1 ls)
parsePair _ = error "parsePair: Malformed input: missing a '('?"

extractParam :: [Token] -> Exp
extractParam (IntToken i:_) = ELit i
extractParam (FloatToken i:_) = EFloat i
extractParam (SymToken Tru:_) = ETrue
extractParam (SymToken Fal:_) = EFalse
extractParam (SymToken NotNumber:_) = NaN
extractParam (SymToken LParen:SymToken Plus:ts) = EAdd (extractParam ts) $ rightParam 1 ts
extractParam (SymToken LParen:SymToken Mul:ts) =  EMul (extractParam ts) $ rightParam 1 ts
extractParam (SymToken LParen:SymToken Sub:ts) =  ESub (extractParam ts) $ rightParam 1 ts
extractParam (SymToken LParen:SymToken Div:ts) =  EDiv (extractParam ts) $ rightParam 1 ts
extractParam (SymToken LParen:SymToken LessT:ts) = ELessEqThan (extractParam ts) $ rightParam 1 ts
extractParam x = error $ "extractParam: Malformed input: are you missing a '('? " ++ (show x)
        
-- Int: number of left parens we are finding a corresponding right paren for (> 0)
rightParam :: Int -> [Token] -> Exp
rightParam _ (_:IntToken j:_) = ELit j
rightParam _ (_:FloatToken j:_) = EFloat j
rightParam _ (_:SymToken Tru:_) = ETrue
rightParam _ (_:SymToken Fal:_) = EFalse
rightParam _ (_:SymToken NotNumber:_) = NaN
rightParam n (SymToken LParen:_:ts) = extractParam $ removeLeftParam n ts
rightParam _ ls= error $ "rightParam: Malformed input: are you missing a paren?" ++ show ls

removeLeftParam :: Int -> [Token] -> [Token]
removeLeftParam 0 ts = ts
removeLeftParam x (SymToken RParen:ts) = removeLeftParam (x-1) ts
removeLeftParam x (SymToken LParen:ts) = removeLeftParam (x+1) ts
removeLeftParam x (_:ts) = removeLeftParam x ts
removeLeftParam _ [] = []

parseResult :: (a, [Token]) -> a
parseResult (a, []) = a
parseResult (_, _)  = error "Parser did not consume entire stream"

data Basic = Basic
    { program :: String } 

basic :: Parser Basic
basic = Basic <$> argument str (metavar "program")

data Token = SymToken Sym | NumToken Number | IntToken Integer | FloatToken Float
    deriving (Show, Eq)

data Sym = LParen | RParen | Plus | Spc | Mul | Sub | Div | If | Tru | Fal | LessT | NotNumber
    deriving (Show, Eq)

data Number = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    deriving (Show, Eq)

numberToInt :: Number -> Integer
numberToInt Zero = 0
numberToInt One = 1
numberToInt Two = 2
numberToInt Three = 3
numberToInt Four = 4
numberToInt Five = 5
numberToInt Six = 6
numberToInt Seven = 7
numberToInt Eight = 8
numberToInt Nine = 9

charToNumber :: Char -> Number
charToNumber '0' = Zero
charToNumber '1' = One
charToNumber '2' = Two
charToNumber '3' = Three
charToNumber '4' = Four
charToNumber '5' = Five
charToNumber '6' = Six
charToNumber '7' = Seven
charToNumber '8' = Eight
charToNumber '9' = Nine
charToNumber _ = undefined

data Exp =
      EAdd Exp Exp 
    | EMul Exp Exp
    | ESub Exp Exp
    | EDiv Exp Exp
    | ELit Integer
    | EFloat Float
    | ELessEqThan Exp Exp
    | EIf Exp Exp Exp
    | ETrue
    | EFalse
    | NaN
    deriving (Show, Eq)

eval :: Exp -> Exp
eval (EAdd (ELit n1) (ELit n2)) = ELit $ n1 + n2
eval (EAdd (EFloat n1) (EFloat n2)) = EFloat $ n1 + n2
eval (EAdd (EFloat _) NaN) = NaN
eval (EAdd NaN (EFloat _)) = NaN
eval (EAdd NaN NaN)        = NaN
eval (EAdd (ELit _) (EFloat _)) = error "Arguments to EAdd do not match in type"
eval (EAdd (EFloat _) (ELit _)) = error "Arguments to EAdd do not match in type"
eval (EAdd NaN _) = error "Arguments to EAdd do not match in type"
eval (EAdd _ NaN) = error "Arguments to EAdd do not match in type"
eval (EAdd e1 e2) = eval $ EAdd (eval e1) (eval e2)

eval (EMul (ELit n1) (ELit n2)) = ELit $ n1 * n2
eval (EMul (EFloat n1) (EFloat n2)) = EFloat $ n1 * n2
eval (EMul (EFloat _) NaN) = NaN
eval (EMul NaN (EFloat _)) = NaN
eval (EMul NaN NaN)        = NaN
eval (EMul (ELit _) (EFloat _)) = error "Arguments to EMul do not match in type"
eval (EMul (EFloat _) (ELit _))  = error "Arguments to EMul do not match in type"
eval (EMul NaN _) = error "Arguments to EMul do not match in type"
eval (EMul _ NaN) = error "Arguments to EMul do not match in type"
eval (EMul e1 e2) = eval $ EMul (eval e1) (eval e2)

eval (ESub (ELit n1) (ELit n2)) = ELit $ n1 - n2
eval (ESub (EFloat n1) (EFloat n2)) = EFloat $ n1 - n2
eval (ESub (EFloat _) NaN) = NaN
eval (ESub NaN (EFloat _)) = NaN
eval (ESub NaN NaN)        = NaN
eval (ESub (ELit _) (EFloat _)) = error "Arguments to ESub do not match in type"
eval (ESub (EFloat _) (ELit _)) = error "Arguments to ESub do not match in type"
eval (ESub NaN _) = error "Arguments to ESub do not match in type"
eval (ESub _ NaN) = error "Arguments to ESub do not match in type"
eval (ESub e1 e2) = eval $ ESub (eval e1) (eval e2)

eval (EDiv (ELit n1) (ELit n2)) 
    | n2 == 0 = error "Integer division by zero"
    | otherwise = ELit $ n1 `div` n2
eval (EDiv (EFloat n1) (EFloat n2)) 
    | n2 == 0 = NaN 
    | otherwise = EFloat $ n1 / n2
eval (EDiv (EFloat _) NaN) = NaN
eval (EDiv NaN (EFloat _)) = NaN
eval (EDiv NaN NaN)        = NaN
eval (EDiv (ELit _) (EFloat _)) = error "Arguments to EDiv do not match in type"
eval (EDiv (EFloat _) (ELit _)) = error "Arguments to EDiv do not match in type"
eval (EDiv NaN _) = error "Arguments to EDiv do not match in type"
eval (EDiv _ NaN) = error "Arguments to EDiv do not match in type"
eval (EDiv e1 e2) = eval $ EDiv (eval e1) (eval e2)

eval (EIf ETrue e1 _)  = eval e1
eval (EIf EFalse _ e2) = eval e2
eval (EIf (ELessEqThan (ELit n1) (ELit n2)) e1 e2) 
    | n1 <= n2 = eval e1
    | n2 <= n1 = eval e2
    | otherwise = error "Invalid expression in ELessEqThan expression"
eval (EIf (ELessEqThan (EFloat n1) (EFloat n2)) e1 e2)
    | n1 <= n2 = eval e1
    | n2 <= n1 = eval e2
    | otherwise = error "Invalid expression in ELessEqThan expression"
eval (EIf (ELessEqThan e1 e2) e3 e4) = eval $ (EIf (ELessEqThan (eval e1) (eval e2)) e3 e4)

eval e1 = e1

evalFinal :: Exp -> IO ()
evalFinal (ELit i) = putStrLn $ show i
evalFinal (EFloat i) = putStrLn $ show i
evalFinal NaN = putStrLn $ show NaN
evalFinal _ = error "Invalid argument"