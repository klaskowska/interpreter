import Control.Monad.Reader
import Evaluation
import ErrM
import ParGrammar
import AbsGrammar
import Data.Map
import System.IO
import System.Environment

wrongNoArgMsg = "Wrong number of arguments"

runInterpreter :: FilePath -> IO (Either Evaluation.Err (Val, (ProgState Val)))
runInterpreter filename = do
  handle <- openFile filename ReadMode
  s <- hGetContents handle
  case pProg $ myLexer s of
    (Bad err) -> do
      putStrLn err
      return (Left err)
      --exitFailure
    (Ok tree) -> do   -- tree to będzie Abstract Syntax Tree dla programu, czyli tree ::  Program
      putStrLn "Parsing status: OK"
      runEvalMonad (evalProg tree) (empty, empty)

-- filename = "good/01-01-int-type.prg"
-- s = "int main() {print_int[1];}"
main :: IO (Either Evaluation.Err (Val, (ProgState Val)))
main = do
  args <- getArgs
  case args of
    [filename] -> runInterpreter filename
    _ -> do
      putStrLn wrongNoArgMsg
      return (Left wrongNoArgMsg)
      