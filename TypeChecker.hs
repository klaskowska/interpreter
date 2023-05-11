module TypeChecker where

import Prelude hiding (lookup)
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Common
import Exception
import ParGrammar
import AbsGrammar

----------------------- Helper functions -----------------------

-- Evaluates type for expressions which are expected to take
-- one expression of type `expectT` as an argument and return value of type `expectT`
evalExprTypeOneArg :: Expr -> Type -> String -> EvalMonad Type Type
evalExprTypeOneArg arg expectT exprStr = do {
  t <- evalExprType arg;
  case t of
    expectT -> return expectT
    otherwise -> throwError (wrongTypeExprOneArg exprStr t expectT)
}

-- Evaluates type for expressions which are expected to take
-- two expressions of type `expectT` as arguments and return value of type `expectT`
evalExprTypeTwoArg :: Expr -> Expr -> Type -> String -> EvalMonad Type Type
evalExprTypeTwoArg arg1 arg2 expectT exprStr = evalExprTypeTwoArgMix arg1 arg2 expectT expectT exprStr

-- Evaluates type for expressions which are expected to take
-- two expressions of type `expectTArg` as arguments and return value of type `expectTRet`
evalExprTypeTwoArgMix :: Expr -> Expr -> Type -> Type -> String -> EvalMonad Type Type
evalExprTypeTwoArgMix arg1 arg2 expectTArg expectTRet exprStr = do {
  t1 <- evalExprType arg1;
  t2 <- evalExprType arg2;
  case (t1, t2) of
    (expectTArg, expectTArg) -> return expectTRet
    (expectTArg, _) -> throwError (wrongTypeExprTwoArg exprStr "second" t2 expectTArg)
    otherwise -> throwError (wrongTypeExprTwoArg exprStr "first" t1 expectTArg)
}

--------------- Type checker for expressions ---------------

evalExprType :: Expr -> EvalMonad Type Type

evalExprType (EVar x) = do {
  (env, store) <- get;
  case lookup x env of
    (Just l) -> return (store ! l);
    _ -> throwError(noVarMsg x);
}

-- arithmetic expressions

evalExprType (EInt _) = return Int

evalExprType (Neg expr) = evalExprTypeOneArg expr Int (show (Neg expr))

evalExprType (EMul expr1 op expr2) = 
  let exprStr = show (EMul expr1 op expr2) in
    evalExprTypeTwoArg expr1 expr2 Int exprStr

evalExprType (EAdd expr1 op expr2) = 
  let exprStr = show (EAdd expr1 op expr2) in
    evalExprTypeTwoArg expr1 expr2 Int exprStr


-- boolean expressions

evalExprType ETrue = return Bool

evalExprType EFalse = return Bool

evalExprType (Not expr) = evalExprTypeOneArg expr Bool (show (Not expr))

evalExprType (ERel expr1 op expr2) = 
  let exprStr = show (ERel expr1 op expr2) in
    evalExprTypeTwoArgMix expr1 expr2 Int Bool exprStr

evalExprType (EAnd expr1 expr2) = 
  let exprStr = show (EAnd expr1 expr2) in
    evalExprTypeTwoArg expr1 expr2 Bool exprStr

evalExprType (EOr expr1 expr2) = 
  let exprStr = show (EOr expr1 expr2) in
    evalExprTypeTwoArg expr1 expr2 Bool exprStr


-- string expressions

evalExpr (EString s) = return Str