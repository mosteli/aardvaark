module Main where

import Options.Applicative
import Data.Semigroup ((<>))

main :: IO ()
main = greetBasic =<< execParser opts
    where
        opts = info (basic <**> helper)
            ( fullDesc
            <> progDesc "Compile nothing at all!"
            <> header "aardvark - a fuzzy animal compiler" )

greetBasic :: Basic -> IO ()
greetBasic (Basic b arguments) 
    | b = printNums arguments
    | otherwise = printArgs arguments
    where
        printArgs :: [String] -> IO () 
        printArgs [] = return ()
        printArgs (a:ls)  = (putStrLn a) >> printArgs ls
        printNums :: [String] -> IO ()
        printNums [] = return ()
        printNums (a:ls) = (putStrLn (show (length a))) >> printNums ls

data Basic = Basic
    { reportLength :: Bool
    , args :: [String] }

basic :: Parser Basic
basic = Basic
    <$> switch
        ( long "length" 
        <> short 'l'
        <> help "Prints the lengths of the arguments")
    <*> many
        (argument str (metavar "argument")) 
    
data Exp =
      EAdd Exp Exp 
    | ELit Int
    deriving (Show)

eval :: Exp -> Exp
eval (EAdd (ELit n) (ELit m)) = ELit $ n + m
eval e = e