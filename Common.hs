module Common where

import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import AbsGrammar

type Var = Ident
type Err = String
type Loc = Int
type Env = Map Var Loc
type Store a = Map Loc a
type ProgState a = (Env, Store a)

-- A monad used to evaluate expressions
type EvalMonad a b = (StateT (ProgState a) (ExceptT Err IO)) b

-- Exacutes expression evaluation and returns unpacked value
runEvalMonad :: (EvalMonad a b) -> ProgState a -> IO (Either Err (b, (ProgState a)))
runEvalMonad v progState = (runExceptT (runStateT v progState))

alloc :: Store a -> Loc
alloc store = size store