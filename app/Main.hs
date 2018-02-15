module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Char

import qualified Parser as C
import Grammar

main :: IO ()
main = putStrLn "In progress..."
-- main = greetBasic =<< execParser opts
--     where
--         opts = info (basic <**> helper)
--             ( fullDesc
--             <> progDesc "Compile nothing at all!"
--             <> header "aardvark - a fuzzy animal compiler" )

-- greetBasic :: Basic -> IO ()
-- greetBasic (Basic p) = evalFinal $ eval $ parsePair $ lx p

data Basic = Basic
    { program :: String }

basic :: Parser Basic
basic = Basic <$> argument str (metavar "program")

eval :: Exp -> Exp
eval (EAdd (EInt n1) (EInt n2)) = EInt $ n1 + n2
eval (EAdd (EFloat n1) (EFloat n2)) = EFloat $ n1 + n2
eval (EAdd (EFloat _) ENaN) = ENaN
eval (EAdd ENaN (EFloat _)) = ENaN
eval (EAdd ENaN ENaN)        = ENaN
eval (EAdd (EInt _) (EFloat _)) = error "Arguments to EAdd do not match in type"
eval (EAdd (EFloat _) (EInt _)) = error "Arguments to EAdd do not match in type"
eval (EAdd ENaN _) = error "Arguments to EAdd do not match in type"
eval (EAdd _ ENaN) = error "Arguments to EAdd do not match in type"
eval (EAdd e1 e2) = eval $ EAdd (eval e1) (eval e2)

eval (EMul (EInt n1) (EInt n2)) = EInt $ n1 * n2
eval (EMul (EFloat n1) (EFloat n2)) = EFloat $ n1 * n2
eval (EMul (EFloat _) ENaN) = ENaN
eval (EMul ENaN (EFloat _)) = ENaN
eval (EMul ENaN ENaN)        = ENaN
eval (EMul (EInt _) (EFloat _)) = error "Arguments to EMul do not match in type"
eval (EMul (EFloat _) (EInt _))  = error "Arguments to EMul do not match in type"
eval (EMul ENaN _) = error "Arguments to EMul do not match in type"
eval (EMul _ ENaN) = error "Arguments to EMul do not match in type"
eval (EMul e1 e2) = eval $ EMul (eval e1) (eval e2)

eval (ESub (EInt n1) (EInt n2)) = EInt $ n1 - n2
eval (ESub (EFloat n1) (EFloat n2)) = EFloat $ n1 - n2
eval (ESub (EFloat _) ENaN) = ENaN
eval (ESub ENaN (EFloat _)) = ENaN
eval (ESub ENaN ENaN)        = ENaN
eval (ESub (EInt _) (EFloat _)) = error "Arguments to ESub do not match in type"
eval (ESub (EFloat _) (EInt _)) = error "Arguments to ESub do not match in type"
eval (ESub ENaN _) = error "Arguments to ESub do not match in type"
eval (ESub _ ENaN) = error "Arguments to ESub do not match in type"
eval (ESub e1 e2) = eval $ ESub (eval e1) (eval e2)

eval (EDiv (EInt n1) (EInt n2))
    | n2 == 0 = error "Integer division by zero"
    | otherwise = EInt $ n1 `div` n2
eval (EDiv (EFloat n1) (EFloat n2))
    | n2 == 0 = ENaN
    | otherwise = EFloat $ n1 / n2
eval (EDiv (EFloat _) ENaN) = ENaN
eval (EDiv ENaN (EFloat _)) = ENaN
eval (EDiv ENaN ENaN)        = ENaN
eval (EDiv (EInt _) (EFloat _)) = error "Arguments to EDiv do not match in type"
eval (EDiv (EFloat _) (EInt _)) = error "Arguments to EDiv do not match in type"
eval (EDiv ENaN _) = error "Arguments to EDiv do not match in type"
eval (EDiv _ ENaN) = error "Arguments to EDiv do not match in type"
eval (EDiv e1 e2) = eval $ EDiv (eval e1) (eval e2)

eval (EIf (EBool True) e1 _)  = eval e1
eval (EIf (EBool False) _ e2) = eval e2
eval (EIf (ELessEqThan (EInt n1) (EInt n2)) e1 e2)
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
evalFinal (EInt i) = putStrLn $ show i
evalFinal (EFloat i) = putStrLn $ show i
evalFinal ENaN = putStrLn $ show ENaN
evalFinal _ = error "Invalid argument"
