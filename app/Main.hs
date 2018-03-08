module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Lexer as L
import qualified Parser as P
import qualified Eval as E

main :: IO ()
main = greetBasic =<< execParser opts
    where
        opts = info (basic <**> helper)
            ( fullDesc
            <> progDesc "Compile some arithmetic expressions!"
            <> header "aardvark - a fuzzy animal compiler" )

greetBasic :: Basic -> IO ()
greetBasic (Basic lex par read step prog)
    | read && lex = do
        input <- readFile prog
        putStrLn $ show $ L.alexScanTokens input
    | read && par = do
        input <- readFile prog
        putStrLn $ show $ P.parser $ L.alexScanTokens input
    | read = do
        input <- readFile prog
        E.evalFinal $ E.deepEval $ P.parser $ L.alexScanTokens input
    | step = E.showExecutionSteps $ P.parser $ lexed
    | lex = putStrLn $ show $ lexed
    | par = if null lexed
        then putStrLn "No tokens were parsed. Did you pass an empty file?" 
        else putStrLn $ show $ P.parser $ lexed
    | otherwise = if null lexed
        then putStrLn "No tokens were parsed. Did you pass an empty file?"
        else E.evalFinal $ E.deepEval $ P.parser $ lexed
    where
        lexed = L.alexScanTokens prog

data Basic = Basic
    { useLexer  :: Bool
    , useParser :: Bool
    , readFromFile :: Bool
    , showSteps :: Bool
    , program   :: String }

basic :: Parser Basic
basic = Basic 
    <$> switch
        ( long "lex"
        <> short 'l'
        <> help "Prints the output of the lexer for a given input.")
    <*> switch
        ( long "parse"
        <> short 'p'
        <> help "Prints the output of the parser for a given input.")
    <*> switch
        ( long "readFile"
        <> short 'r'
        <> help "Reads as input from the file path given.")
    <*> switch
        ( long "step"
        <> short 's'
        <> help "Evalutates input, printing each step of evaluation along the way.")
    <*> argument str (metavar "program")