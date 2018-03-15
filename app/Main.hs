{-# LANGUAGE BangPatterns #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Lexer as L
import qualified Parser as P
import qualified Eval as E
import qualified Grammar as G
import qualified Typechecker as TC

main :: IO ()
main = evaluator =<< execParser opts
    where
        opts = info (basic <**> helper)
            ( fullDesc
            <> progDesc "Compile some arithmetic expressions!"
            <> header "aardvark - a fuzzy animal compiler" )

evaluator :: Basic -> IO ()
evaluator (Basic lex par read step prog)
    | read && lex = do
        input <- readFile prog
        putStrLn $ show $ L.alexScanTokens input
    | read && par = do
        input <- readFile prog
        putStrLn $ show $ parse input
    | read && step = do
        input <- readFile prog 
        stepTypecheckedEval $ parse input
    | read = E.evalFinal $ stringToTC prog 
    | step = stepTypecheckedEval $ parse prog 
    | lex = putStrLn $ show $ lexed
    | par = putStrLn $ show $ parse prog 
    | otherwise = E.evalFinal $ stringToTC prog 
    where
        lexed = L.alexScanTokens prog
        parse :: String -> G.Exp 
        parse s = P.parser $ L.alexScanTokens s
        typecheckEval e = evaluate (TC.typecheck' e) e
        stringToTC = typecheckEval . parse 
        stepTypecheckedEval e = stepEvaluate (TC.typecheck' e) e 

evaluate :: G.YType -> G.Exp -> G.Exp 
evaluate !t e = E.deepEval' [] e 

stepEvaluate :: G.YType -> G.Exp -> IO () 
stepEvaluate !t e = E.showExecutionSteps [] e

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