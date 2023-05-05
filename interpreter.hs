import Control.Monad.Reader
import Evaluation
import ErrM
import ParGrammar
import AbsGrammar
import Data.Map
import Control.Applicative.Lift

s = "int main() {print_int[1];}"
main :: IO (Either Evaluation.Err (Val, (Env, Locs Val)))
main = do -- dzieje sie pod monadą IO
  case pProg $ myLexer s of
    (Bad err) -> do
      putStrLn err
      return (Left err)
      --exitFailure
    (Ok tree) -> do   -- tree to będzie Abstract Syntax Tree dla programu, czyli tree ::  Program
      putStrLn "Parsing status: OK"
      runEvalMonad (evalProg tree) empty empty
      