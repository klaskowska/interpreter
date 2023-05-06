import Control.Monad.Reader
import Evaluation
import ErrM
import ParGrammar
import AbsGrammar
import Data.Map
import System.IO
import System.Environment

wrongNoArgMsg = "Wrong number of arguments"

runInterpreter :: FilePath -> IO (Either Evaluation.Err (RetObj, (ProgState Val)))
runInterpreter filename = do
  handle <- openFile filename ReadMode
  s <- hGetContents handle
  case pProg $ myLexer s of
    (Bad err) -> do
      putStrLn err
      return (Left err)
      --exitFailure
    (Ok tree) -> do   -- tree to będzie Abstract Syntax Tree dla programu, czyli tree ::  Program
      progRes <- runEvalMonad (evalProg tree) (empty, empty);
      case progRes of
        (Left progErr) -> do
          hPutStrLn stderr progErr;
          return (Left progErr);
        otherwise -> return progRes;

-- filename = "good/01-01-int-type.prg"
-- s = "int main() {print_int[1];}"
main :: IO (Either Evaluation.Err (RetObj, (ProgState Val)))
main = do
  args <- getArgs
  case args of
    [filename] -> runInterpreter filename
    _ -> do
      putStrLn wrongNoArgMsg
      putStrLn (show (length args))
      return (Left wrongNoArgMsg)
      