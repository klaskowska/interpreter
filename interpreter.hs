-- TODO
-- 1. Generalize arithmetic operations


import Prelude hiding (lookup)
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

import ParGrammar
import AbsGrammar

-- Error messages
noVarMsg :: Ident -> String
noVarMsg x = "Unknown variable " ++ (show x) ++ " at some place"
divZeroMsg = "Divide by zero"

-- Types used in interpreter
data Val = VInt Int | VBool Bool | VString String | VVoid
type Var = Ident
type Err = String
type Loc = Int
type Env = Map Var Loc
type Locs a = Map Loc a

-- A monad used to evaluate expressions
type EvalMonad a = (StateT (Locs a) (ReaderT Env (ExceptT Err IO))) a

-- Exacutes expression evaluation and returns unpacked value
runEvalMonad :: (EvalMonad a) -> Env -> Locs a -> IO (Either Err (a, Locs a))
runEvalMonad v env locs = (runExceptT (runReaderT (runStateT v locs) env))

----------------------- Helper functions -----------------------
alloc :: Locs a -> Loc
alloc locs = size locs

putStrLnM :: String -> (StateT (Locs a) (ReaderT Env (ExceptT Err IO))) ()
putStrLnM s = lift $ (lift $ (lift $ putStrLn s));

-- if `isDiv` and `aExpr2` evaluates to 0 then throws exception
evalArithm :: (Int -> Int -> Int) -> Expr -> Expr -> Bool -> EvalMonad Val
evalArithm op aExpr1 aExpr2 isDiv = do {
  (VInt n1) <- evalExpr aExpr1;
  (VInt n2) <- evalExpr aExpr2;
  if isDiv && n2 == 0
    then throwError(divZeroMsg);
    else return (VInt (op n1 n2));
}

evalRel :: (Int -> Int -> Bool) -> Expr -> Expr -> EvalMonad Val
evalRel op aExpr1 aExpr2 = do {
  (VInt n1) <- evalExpr aExpr1;
  (VInt n2) <- evalExpr aExpr2;
  return (VBool (op n1 n2));
}

evalBool :: (Bool -> Bool -> Bool) -> Expr -> Expr -> EvalMonad Val
evalBool op bExpr1 bExpr2 = do {
  (VBool b1) <- evalExpr bExpr1;
  (VBool b2) <- evalExpr bExpr2;
  return (VBool (op b1 b2));
}

--------------- Functions evaluating expressions ---------------

evalExpr :: Expr -> EvalMonad Val

-- arithmetic expressions

evalExpr (EVar x) = do {
  env <- ask;
  locs <- get;
  let lMaybe = lookup x env in
    case lMaybe of
      (Just l) -> return (locs ! l);
      _ -> throwError(noVarMsg x);
}

evalExpr (EInt n) = return (VInt (fromIntegral n))

evalExpr (Neg aExpr) = do {
  (VInt n) <- evalExpr aExpr;
  return (VInt (negate n));
}

evalExpr (EMul aExpr1 Times aExpr2) = evalArithm (*) aExpr1 aExpr2 False

evalExpr (EMul aExpr1 Div aExpr2) = evalArithm div aExpr1 aExpr2 True

evalExpr (EMul aExpr1 Mod aExpr2) = evalArithm mod aExpr1 aExpr2 True

evalExpr (EAdd aExpr1 Plus aExpr2) = evalArithm (+) aExpr1 aExpr2 False

evalExpr (EAdd aExpr1 Minus aExpr2) = evalArithm (-) aExpr1 aExpr2 False

-- boolean expressions

evalExpr ETrue = return (VBool True)

evalExpr EFalse = return (VBool False)

evalExpr (Not bExpr) = do {
  (VBool b) <- evalExpr bExpr;
  return (VBool (not b));
}

evalExpr (ERel aExpr1 LTH aExpr2) = evalRel (<) aExpr1 aExpr2

evalExpr (ERel aExpr1 LE aExpr2) = evalRel (<=) aExpr1 aExpr2

evalExpr (ERel aExpr1 GTH aExpr2) = evalRel (>) aExpr1 aExpr2

evalExpr (ERel aExpr1 GE aExpr2) = evalRel (>=) aExpr1 aExpr2

evalExpr (ERel aExpr1 EQU aExpr2) = evalRel (==) aExpr1 aExpr2

evalExpr (ERel aExpr1 NE aExpr2) = evalRel (/=) aExpr1 aExpr2

evalExpr (EAnd bExpr1 bExpr2) = evalBool (&&) bExpr1 bExpr2

evalExpr (EOr bExpr1 bExpr2) = evalBool (||) bExpr1 bExpr2


-- string expressions

evalExpr (EString s) = return (VString s)