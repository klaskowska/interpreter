-- TODO:
-- 1. W wyrażeniach jednak możesz robić generyczne sprawdzanie typów, bo starczy t == t1 zamiast case of
-- 2. sprawdź sygnaturę main

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
type Env = Map Var Type
type TType = Type' ()
-- Object which can be returned by statement
data RetObj = RType TType | NoRet

-- A monad used to evaluate type of program components
type EvalTypeMonad a = (StateT Env (ExceptT Err IO)) a

-- Exacutes expression evaluation and returns unpacked value
runEvalTypeMonad :: (EvalTypeMonad a) -> Env -> IO (Either Err (a, Env))
runEvalTypeMonad v env = (runExceptT (runStateT v env))


----------------------- Helper functions -----------------------

typeToTType :: Type' a -> TType
typeToTType (Int _) = Int ()
typeToTType (Str _) = Str ()
typeToTType (Bool _) = Bool ()
typeToTType (Void _) = Void ()
typeToTType (FuncType _ retT argTs) = 
  FuncType () (typeToTType retT) (Prelude.map (\t -> typeToTType t) argTs)

eqType :: Type' a -> Type' b -> Bool
eqType (Int _) (Int _) = True
eqType (Str _) (Str _) = True
eqType (Bool _) (Bool _) = True
eqType (Void _) (Void _) = True
eqType (FuncType _ retT1 argTs1) (FuncType _ retT2 argTs2) = 
  eqType retT1 retT2 && (length argTs1) == (length argTs2) &&
    (and $ zipWith eqType argTs1 argTs2)
eqType _ _ = False

-- Evaluates type for expressions which are expected to take
-- two expressions of type Int as arguments and return value of type `retType`
evalExprTypeArithm :: BNFC'Position -> Expr -> Expr -> TType -> String -> EvalTypeMonad TType
evalExprTypeArithm pos arg1 arg2 retType exprName = do {
  t1 <- evalExprType arg1;
  t2 <- evalExprType arg2;
  case (t1, t2) of
    (Int _, Int _) -> return retType
    (Int _, _) -> throwError (errorMsg pos (wrongTypeExprTwoArg exprName "second" t2 (Int ())))
    otherwise -> throwError (errorMsg pos (wrongTypeExprTwoArg exprName "first" t1 (Int ())))
}

-- Evaluates type for expressions which are expected to take
-- two expressions of type Bool as arguments and return value of type Bool
evalExprTypeBool :: BNFC'Position -> Expr -> Expr -> String -> EvalTypeMonad TType
evalExprTypeBool pos arg1 arg2 exprName = do {
  t1 <- evalExprType arg1;
  t2 <- evalExprType arg2;
  case (t1, t2) of
    (Bool _, Bool _) -> return (Bool ())
    (Bool _, _) -> throwError (errorMsg pos (wrongTypeExprTwoArg exprName "second" t2 (Bool ())))
    otherwise -> throwError (errorMsg pos (wrongTypeExprTwoArg exprName "first" t1 (Bool ())))
}

unpackArg :: Arg -> (Type, Var)
unpackArg (ArgVal _ t x) = (t, x)
unpackArg (ArgRef _ t x) = (t, x)

argsToTypes :: [Arg] -> [Type]
argsToTypes args = Prelude.map (\ arg -> fst (unpackArg arg)) args

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
checkTypesList :: BNFC'Position -> [Expr] -> [TType] -> Ident -> Int -> EvalTypeMonad ()
checkTypesList _ [] [] _ _ = return ()
checkTypesList pos (exprH:exprT) (typeH:typeT) f argPos = do {
  exprType <- evalExprType exprH;
  if exprType == typeH
    then checkTypesList pos exprT typeT f (argPos + 1);
    else throwError (errorMsg pos (wrongPassedArg f argPos exprType typeH));
}

-- Return type based on types returned in two statements
retType :: BNFC'Position -> RetObj -> RetObj -> EvalTypeMonad RetObj
retType _ NoRet t = return t;
retType _ t NoRet = return t;
retType pos (RType t1) (RType t2) = do {
  if t1 == t2
    then return (RType t1);
    else throwError (errorMsg pos diffRet);
}

-- Evaluates statement, but leaves environment whitout variables declared in this statement
evalScopeType :: Stmt -> EvalTypeMonad RetObj
evalScopeType stmt = do {
  globalEnv <- get;
  retObj <- evalStmtType stmt;
  put globalEnv;
  return retObj;
}

-- Returns type which should be returned at the end of function
finalRetType :: RetObj -> TType
finalRetType retObj = 
  case retObj of
    NoRet -> Void ()
    (RType t) -> t

-- Checks types for statements in a function body when `args` are added to the environment
-- and checks if function returns value of type `t`
checkFuncBody :: BNFC'Position -> [Arg] -> Type -> Block -> EvalTypeMonad ()
checkFuncBody pos args t block = do {
  globalEnv <- get;
  setParams args;
  retObj <- evalStmtType (BlockStmt pos block);
  let retType = finalRetType retObj in
    if eqType retType t
      then put globalEnv;
      else throwError (errorMsg pos (wrongTypeLambda retType t));     
}

--------------- Type checker for expressions ---------------

evalExprType :: Expr -> EvalTypeMonad TType

evalExprType (EVar pos x) = do {
  env <- get;
  case lookup x env of
    (Just t) -> return (typeToTType t);
    _ -> throwError (errorMsg pos (noVarMsg x));
}

evalExprType (ECallFunc pos f args) = do {
  fType <- evalExprType (EVar pos f);
  case fType of
    (FuncType () retType argTypes) -> do {
      if (length args) /= (length argTypes)
        then throwError (errorMsg pos (wrongArgNumb f));
        else do {
          checkTypesList pos args argTypes f 1;
          return retType;
        }
    }
    otherwise -> throwError (errorMsg pos (notFunc f));
}

evalExprType (ELambda pos args t block) = do {
  checkFuncBody pos args t block;
  let argTypes = argsToTypes args in
    let argTTypes = Prelude.map (\argT -> typeToTType argT) argTypes in
      return (FuncType () (typeToTType t) argTTypes);
}

-- arithmetic expressions

evalExprType (EInt _ _) = return (Int ())

evalExprType (Neg pos expr) = do {
  t <- evalExprType expr;
  case t of
    (Int _) -> return (Int ())
    otherwise -> throwError (errorMsg pos (wrongTypeExprOneArg "negation" t (Int ())))
}

evalExprType (EMul pos expr1 (Times _) expr2) = evalExprTypeArithm pos expr1 expr2 (Int ()) "(*)"

evalExprType (EMul pos expr1 (Div _) expr2) = evalExprTypeArithm pos expr1 expr2 (Int ()) "(/)"

evalExprType (EMul pos expr1 (Mod _) expr2) = evalExprTypeArithm pos expr1 expr2 (Int ()) "mod"

evalExprType (EAdd pos expr1 (Plus _) expr2) = evalExprTypeArithm pos expr1 expr2 (Int ()) "(+)"

evalExprType (EAdd pos expr1 (Minus _) expr2) = evalExprTypeArithm pos expr1 expr2 (Int ()) "(-)"

-- boolean expressions

evalExprType (ETrue _) = return (Bool ())

evalExprType (EFalse _) = return (Bool ())

evalExprType (Not pos expr) = do {
  t <- evalExprType expr;
  case t of
    (Bool _) -> return (Bool ())
    otherwise -> throwError (errorMsg pos (wrongTypeExprOneArg "not" t (Bool ())))
}

evalExprType (ERel pos expr1 (LTH _) expr2) = evalExprTypeArithm pos expr1 expr2 (Bool ()) "(<)"

evalExprType (ERel pos expr1 (LE _) expr2) = evalExprTypeArithm pos expr1 expr2 (Bool ()) "(<=)"

evalExprType (ERel pos expr1 (GTH _) expr2) = evalExprTypeArithm pos expr1 expr2 (Bool ()) "(>)"

evalExprType (ERel pos expr1 (GE _) expr2) = evalExprTypeArithm pos expr1 expr2 (Bool ()) ">"

evalExprType (ERel pos expr1 (EQU _) expr2) = evalExprTypeArithm pos expr1 expr2 (Bool ()) "=="

evalExprType (ERel pos expr1 (NE _) expr2) = evalExprTypeArithm pos expr1 expr2 (Bool ()) "!="

evalExprType (EAnd pos expr1 expr2) = evalExprTypeBool pos expr1 expr2 "&&"

evalExprType (EOr pos expr1 expr2) = evalExprTypeBool pos expr1 expr2 "||"


-- string expressions

evalExprType (EString _ s) = return (Str ())


--------------- Function evaluating statements ---------------
evalStmtType :: Stmt -> EvalTypeMonad RetObj

evalStmtType (RetVoid _) = return (RType (Void ()))

evalStmtType (Ret _ expr) = do {
  t <- evalExprType expr;
  return (RType t);
}

evalStmtType (SEmpty _) = return NoRet

evalStmtType (PrintStr pos expr) = do {
  t <- evalExprType expr;
  case t of
    (Str _) -> return NoRet;
    otherwise -> throwError (errorMsg pos (wrongTypeStmtOneArg "print_str" t (Str ())))
}

evalStmtType (PrintInt pos expr) = do {
  t <- evalExprType expr;
  case t of
    (Int _) -> return NoRet;
    otherwise -> throwError (errorMsg pos (wrongTypeStmtOneArg "print_int" t (Int ())))
}

evalStmtType (StmtExpr _ expr) = do {
  evalExprType expr;
  return NoRet;
}

evalStmtType (CallFunc pos f args) = do {
  evalExprType (ECallFunc pos f args);
  return NoRet;
}

evalStmtType (While pos expr stmt) = do {
  exprType <- evalExprType expr;
  if exprType /= Bool ()
    then throwError (errorMsg pos (wrongTypeWhile exprType (Bool ())));
    else evalScopeType stmt;
}

evalStmtType (IfElse pos expr stmt1 stmt2) = do {
  exprType <- evalExprType expr;
  if exprType /= Bool ()
    then throwError (errorMsg pos (wrongTypeIf exprType (Bool ())));
    else do {
      t1 <- evalScopeType stmt1;
      t2 <- evalScopeType stmt2;
      retType pos t1 t2;
    }
} 

evalStmtType (If pos expr stmt) = do {
  exprType <- evalExprType expr;
  if exprType /= Bool ()
    then throwError (errorMsg pos (wrongTypeIf exprType (Bool ())));
    else evalScopeType stmt;
}

evalStmtType (Ass pos x expr) = do {
  env <- get;
  case lookup x env of
    (Just t) -> do {
      exprType <- evalExprType expr;
      if eqType t exprType
        then return NoRet;
        else throwError (errorMsg pos (wrongTypeAss (show expr) exprType t));
    }
    _ -> throwError (errorMsg pos (noVarMsg x));
}

evalStmtType (VarDef _ t (NoInit _ x)) = do {
  env <- get;
  put (Data.Map.insert x t env);
  return NoRet;
}

evalStmtType (VarDef pos t (Init _ x expr)) = do {
  exprType <- evalExprType expr;
  if eqType exprType t
    then do {
      env <- get;
      put (Data.Map.insert x t env);
      return NoRet;
    }
    else throwError (errorMsg pos (wrongTypeInit exprType t));

}

evalStmtType (BlockStmt _ (Block _ [])) = return NoRet;
evalStmtType (BlockStmt pos (Block _ (stmtH:stmtT))) = do {
  retObjH <- evalStmtType stmtH;
  retObjT <- evalStmtType (BlockStmt pos (Block pos stmtT));
  retType pos retObjH retObjT;
}


--------------- Functions evaluating program types ---------------
addFuncDefs :: [FuncDef] -> EvalTypeMonad ()
addFuncDefs [] = return ()
addFuncDefs ((FuncDef pos t f args _):funcT) = do {
  env <- get;
  if member f env
    then throwError (errorMsg pos (repeatedFunMsg f));
    else do {
      put (Data.Map.insert f (FuncType pos t (argsToTypes args)) env);
      addFuncDefs funcT;
    }
}

checkFuncBodies :: [FuncDef] -> EvalTypeMonad ()
checkFuncBodies [] = return ();
checkFuncBodies ((FuncDef pos t _ args stmt):funcT) = do {
  checkFuncBody pos args t stmt;
  checkFuncBodies funcT;
}

evalProgType :: Prog -> EvalTypeMonad ()
evalProgType (Prog _ funcDefs) = do {
  addFuncDefs funcDefs;
  checkFuncBodies funcDefs;
}