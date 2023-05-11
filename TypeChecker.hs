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
-- two expressions of type Int as arguments and return value of type `retType`
evalExprTypeArithm :: Expr -> Expr -> Type -> String -> EvalMonad Type Type
evalExprTypeArithm arg1 arg2 retType exprStr = do {
  t1 <- evalExprType arg1;
  t2 <- evalExprType arg2;
  case (t1, t2) of
    (Int, Int) -> return retType
    (Int, _) -> throwError (wrongTypeExprTwoArg exprStr "second" t2 Int)
    otherwise -> throwError (wrongTypeExprTwoArg exprStr "first" t1 Int)
}

-- Evaluates type for expressions which are expected to take
-- two expressions of type Bool as arguments and return value of type Bool
evalExprTypeBool :: Expr -> Expr -> String -> EvalMonad Type Type
evalExprTypeBool arg1 arg2 exprStr = do {
  t1 <- evalExprType arg1;
  t2 <- evalExprType arg2;
  case (t1, t2) of
    (Bool, Bool) -> return Bool
    (Bool, _) -> throwError (wrongTypeExprTwoArg exprStr "second" t2 Bool)
    otherwise -> throwError (wrongTypeExprTwoArg exprStr "first" t1 Bool)
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

evalExprType (Neg expr) = do {
  t <- evalExprType expr;
  case t of
    Int -> return Int
    otherwise -> throwError (wrongTypeExprOneArg (show (Neg expr)) t Int)
}

evalExprType (EMul expr1 op expr2) = evalExprTypeArithm expr1 expr2 Int (show (EMul expr1 op expr2))

evalExprType (EAdd expr1 op expr2) = evalExprTypeArithm expr1 expr2 Int (show (EAdd expr1 op expr2))


-- boolean expressions

evalExprType ETrue = return Bool

evalExprType EFalse = return Bool

evalExprType (Not expr) = do {
  t <- evalExprType expr;
  case t of
    Bool -> return Bool
    otherwise -> throwError (wrongTypeExprOneArg (show (Not expr)) t Bool)
}

evalExprType (ERel expr1 op expr2) = evalExprTypeArithm expr1 expr2 Bool (show (ERel expr1 op expr2))

evalExprType (EAnd expr1 expr2) = evalExprTypeBool expr1 expr2 (show (EAnd expr1 expr2))

evalExprType (EOr expr1 expr2) = evalExprTypeBool expr1 expr2 (show (EOr expr1 expr2))


-- string expressions

evalExpr (EString s) = return Str