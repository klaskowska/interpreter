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

evalStmt (CallFunc f args) = do {
  evalExprType (ECallFunc f args);
  return NoRet;
}

evalStmt (While expr stmt) = do {
  exprType <- evalExprType expr;
  if b
    then do {
      retObj <- evalStmt stmt;
      case retObj of
        NoRet -> evalStmt (While bExpr stmt);
        otherwise -> return retObj;
    }
    else
      return NoRet;
}

evalStmt (IfElse bExpr stmt1 stmt2) = do {
  (VBool b) <- evalExpr bExpr;
  if b
    then evalStmt stmt1;
    else evalStmt stmt2;
} 

evalStmt (If bExpr stmt) = do {
  (VBool b) <- evalExpr bExpr;
  if b
    then evalStmt stmt;
    else return NoRet;  
}

evalStmt (Ass x expr) = do {
  (env, _) <- get;
  let lMaybe = lookup x env in
    case lMaybe of
      (Just l) -> do {
        n <- evalExpr expr;
        (env1, store1) <- get;   -- read env again in case of changes during expr evaluation
        put (env1, Data.Map.insert l n store1);
        return NoRet;
      }
      _ -> throwError(noVarMsg x);
}

evalStmt (VarDef t (NoInit x)) = do {
  (env, store) <- get;
  newLoc <- return (alloc store);
  put (Data.Map.insert x newLoc env, Data.Map.insert newLoc (NotInit t) store);
  return NoRet;
}

evalStmt (VarDef _ (Init x expr)) = do {
  newState <- initVar x expr;
  put (newState);
  return NoRet;
}

evalStmtType (BlockStmt (Block [])) = return NoRet;
evalStmtType (BlockStmt (Block (stmtH:stmtT))) = do {
  retObjH <- evalStmtType stmtH;
  retObjT <- evalStmtType (BlockStmt (Block stmtT))
  case (retObjH, retObjT) of
    (NoRet, t) -> return t;
    (t, NoRet) -> return t;
    (t1, t2) -> if t1 == t2
      then return t1;
      else throwError (diffRet);
}