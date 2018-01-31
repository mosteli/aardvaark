module Main where
    
import Lib
import qualified System.Process as S
import qualified System.Exit as E

main :: IO ()
main = do
    ec <- S.system "./test/tests.sh"
    E.exitWith ec