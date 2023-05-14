import Control.Monad.Reader
import Evaluation
import TypeChecker (runEvalTypeMonad, evalProgType)
import ErrM
import ParGrammar
import AbsGrammar
import Data.Map
import System.IO
import System.Environment
import Control.Monad.Except

wrongNumbArgMsg gvn exp = "Given wrong number of arguments to the interpreter. Given: " ++ (show gvn) ++ ", expected: " ++ (show exp)

getSyntaxTree :: FilePath -> IO (ErrM.Err Prog)
getSyntaxTree filename = do
  handle <- openFile filename ReadMode
  s <- hGetContents handle
  return (pProg $ myLexer s)

runInterpreter :: FilePath -> IO (Either String ())
runInterpreter filename = do
  prog <- getSyntaxTree filename
  case prog of
    (Bad err) -> return (Left err)
    (Ok prog) -> do
      progTypeCheck <- runEvalTypeMonad (evalProgType prog) empty;
      case progTypeCheck of
        (Left errType) -> return (Left errType)
        otherwise -> do
          progRes <- runEvalMonad (evalProg prog) (empty, empty);
          case progRes of
            (Left progErr) -> return (Left progErr)
            otherwise -> return (Right ());

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      interpRes <- runInterpreter filename;
      case interpRes of
        (Left err) -> hPutStrLn stderr err
        (Right _) -> return ()
    _ -> hPutStrLn stderr (wrongNumbArgMsg (length args) 1)
      