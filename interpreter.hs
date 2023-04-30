import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

import ParGrammar
import AbsGrammar

-- types used in interpreter
data Val = VInt Int | VBool Bool | VString String | VVoid
type Var = Ident
type Err = String
type Loc = Int
type Env = Map Var Loc
type Locs a = Map Loc a

-- monad used to evaluate expressions
type EvalMonad a b = (StateT (Locs a) (ReaderT Env (ExceptT Err IO))) b

-- exacutes expression evaluation and returns unpacked value
runEvalMonad :: (EvalMonad a b) -> Env -> Locs a -> IO (Either Err (b, Locs a))
runEvalMonad v env locs = (runExceptT (runReaderT (runStateT v locs) env))

-- helper functions
alloc :: Locs a -> Loc
alloc locs = size locs

putStrLnM :: String -> (StateT (Locs a) (ReaderT Env (ExceptT Err IO))) ()
putStrLnM s = lift $ (lift $ (lift $ putStrLn s));