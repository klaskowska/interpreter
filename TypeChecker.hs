-- TODO:
-- 1. W wyrażeniach jednak możesz robić generyczne sprawdzanie typów, bo starczy t == t1 zamiast case of

module TypeChecker where

import Prelude hiding (lookup)
import Data.Map
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Exception
import ParGrammar
import AbsGrammar

type Var = Ident
type Err = String
data Env = Map Var Type
-- Object which can be returned by statement
data RetObj = RType Type | NoRet

-- A monad used to evaluate type of program components
type EvalTypeMonad a = (StateT Env (ExceptT Err IO)) a

-- Exacutes expression evaluation and returns unpacked value
runEvalTypeMonad :: (EvalTypeMonad a) -> Env -> IO (Either Err (a, Env))
runEvalTypeMonad v env = (runExceptT (runStateT v env))


----------------------- Helper functions -----------------------


-- Evaluates type for expressions which are expected to take
-- two expressions of type Int as arguments and return value of type `retType`
evalExprTypeArithm :: Expr -> Expr -> Type -> String -> EvalTypeMonad Type
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
evalExprTypeBool :: Expr -> Expr -> String -> EvalTypeMonad Type
evalExprTypeBool arg1 arg2 exprStr = do {
  t1 <- evalExprType arg1;
  t2 <- evalExprType arg2;
  case (t1, t2) of
    (Bool, Bool) -> return TBool
    (Bool, _) -> throwError (wrongTypeExprTwoArg exprStr "second" t2 Bool)
    otherwise -> throwError (wrongTypeExprTwoArg exprStr "first" t1 Bool)
}

unpackArg :: Arg -> (Type, Var)
unpackArg (ArgVal t x) = (t, x)
unpackArg (ArgRef t x) = (t, x)

argsToTypes :: [Arg] -> [Type]
argsToTypes args = map (arg -> fst (unpackArg arg)) args

setParams :: [Arg] -> EvalTypeMonad ()
setParams [] = return ();
setParams (argH:argT) = do {
  env <- get;
  let (t, x) = unpackArg argH in
    put (Data.Map.insert x t env);
  setParams argT; 
}

-- Checks if types of expressions match expected types
-- Assumes that given lists are the same length
checkTypesList :: [Expr] -> [Type] -> Ident -> Int -> EvalTypeMonad ()
checkTypesList [] [] _ _ = return ()
checkTypesList (exprH:exprT) (typeH:typeT) f argPos = do {
  exprType <- evalExprType exprH;
  if exprType /= typeH
    then throwError (wrongPassedArg f argPos exprType typeH);
  checkTypesList exprT typeT f (argPos + 1);
}

-- Return type based on types returned in two statements
retType :: RetObj -> RetObj -> EvalTypeMonad RetObj
retType (NoRet, t) = return t;
retType (t, NoRet) = return t;
retType (t1, t2) = do {
  if t1 == t2
    then return t1;
    else throwError (diffRet);
}

-- Evaluates statement, but leaves environment whitout variables declared in this statement
evalScopeType :: Stmt -> EvalTypeMonad RetObj
evalScopeType stmt = do {
  globalEnv <- get;
  retObj <- evalStmtType stmt;
  put globalEnv;
  return retObj;
}

--------------- Type checker for expressions ---------------

evalExprType :: Expr -> EvalTypeMonad Type

evalExprType (EVar x) = do {
  env <- get;
  case lookup x env of
    (Just t) -> t;
    _ -> throwError(noVarMsg x);
}

evalExprType (ECallFunc f args) = do {
  fType <- evalExpr (EVar f);
  case fType of
    (FuncType retType argTypes) -> do {
      if (length args) /= (length argTypes)
        then throwError (wrongArgNumb f);
      checkTypesList args argTypes f 1;
      return retType;
    }
    otherwise -> throwError (notFunc f);
}

evalExprType (ELambda args t block) = do {
  globalEnv <- get;
  setParams args;
  retObj <- evalStmtType block;
  let retType = 
    case retObj of
      NoRet -> Void
      (RType type) -> type
      in
      if retType /= t
        then throwError (wrongTypeLambda (show expr) retObj t);
  put globalEnv;
  return (FuncType t (argsToTypes args));
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
    TBool -> return Bool
    otherwise -> throwError (wrongTypeExprOneArg (show (Not expr)) t Bool)
}

evalExprType (ERel expr1 op expr2) = evalExprTypeArithm expr1 expr2 Bool (show (ERel expr1 op expr2))

evalExprType (EAnd expr1 expr2) = evalExprTypeBool expr1 expr2 (show (EAnd expr1 expr2))

evalExprType (EOr expr1 expr2) = evalExprTypeBool expr1 expr2 (show (EOr expr1 expr2))


-- string expressions

evalExprType (EString s) = return Str


--------------- Function evaluating statements ---------------
evalStmtType :: Stmt -> EvalTypeMonad Type

evalStmtType RetVoid = return (RType Void)

evalStmtType (Ret expr) = do {
  t <- evalExprType expr;
  return (RType t);
}

evalStmtType SEmpty = return NoRet

evalStmtType (PrintStr expr) = do {
  t <- evalExprType expr;
  case t of
    Str -> return NoRet;
    otherwise -> throwError (wrongTypeStmtOneArg (show (PrintStr expr)) t Str)
}

evalStmtType (PrintInt expr) = do {
  t <- evalExprType expr;
  case t of
    Int -> return NoRet;
    otherwise -> throwError (wrongTypeStmtOneArg (show (PrintInt expr)) t Int)
}

evalStmtType (StmtExpr expr) = do {
  evalExprType expr;
  return NoRet;
}

evalStmtType (CallFunc f args) = do {
  evalExprType (ECallFunc f args);
  return NoRet;
}

evalStmtType (While expr stmt) = do {
  exprType <- evalExprType expr;
  if exprType /= Bool
    then throwError (wrongTypeWhile (show expr) exprType Bool);
  evalScopeType stmt;
}

evalStmtType (IfElse expr stmt1 stmt2) = do {
  exprType <- evalExprType expr;
  if exprType /= Bool
    then throwError (wrongTypeIf (show expr) exprType Bool);
  t1 <- evalScopeType stmt1;
  t2 <- evalScopeType stmt2;
  retType t1 t2;
} 

evalStmtType (If expr stmt) = do {
  exprType <- evalExprType expr;
  if exprType /= Bool
    then throwError (wrongTypeIf (show expr) exprType Bool);
  evalScopeType stmt;
}

evalStmtType (Ass x expr) = do {
  env <- get;
  case lookup x env of
    (Just t) -> do {
      exprType <- evalExprType expr;
      if t /= exprType
        then throwError (wrongTypeAss (show expr) exprType t);
      return NoRet;
    }
    _ -> throwError(noVarAss x);
}

evalStmtType (VarDef t (NoInit x)) = do {
  env <- get;
  put (Data.Map.insert x t env);
  return NoRet;
}

evalStmtType (VarDef t (Init x expr)) = do {
  exprType <- evalExprType expr;
  if exprType /= t
    then throwError (wrongTypeInit (show expr) exprType t);
  env <- get;
  put (Data.Map.insert x t env);
  return NoRet;
}

evalStmtType (BlockStmt (Block [])) = return NoRet;
evalStmtType (BlockStmt (Block (stmtH:stmtT))) = do {
  retObjH <- evalStmtType stmtH;
  retObjT <- evalStmtType (BlockStmt (Block stmtT));
  retType retObjH retObjT;
}
