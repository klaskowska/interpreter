-- TODO
-- 1. Generalize arithmetic operations
-- 2. Maybe implement return as a throwing exception to break or add another Val type as NotReturned and then use if
-- 3. Remeber to check if NotInit matches assinging in type checker
-- 4. Maybe use type Env = (Env, Locs)

-- QUESTIONS
-- 1. Do I have to check if x is in the env if I have already checked it during static typing?
module Evaluation where

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
repeatedFunMsg = "Repeated name of function in a global function definition"
badMainTypeMsg t = "The type of main function is not Int. Given type: " ++ (show t)
argsMainMsg args = "There were given some arguments in main function definition. Given arguments: " ++ (show args)

-- Types used in interpreter
data Val = VInt Int | VBool Bool | VString String | VVoid | VFunc (FuncDef, (Env, Locs Val)) | NotInit Type
type Var = Ident
type Err = String
type Loc = Int
type Env = Map Var Loc
type Locs a = Map Loc a

-- A monad used to evaluate expressions
type EvalMonad a = (StateT (Env, Locs a) (ExceptT Err IO)) a

-- Exacutes expression evaluation and returns unpacked value
runEvalMonad :: (EvalMonad a) -> Env -> Locs a -> IO (Either Err (a, (Env, Locs a)))
runEvalMonad v env locs = (runExceptT (runStateT v (env, locs)))

----------------------- Helper functions -----------------------
alloc :: Locs a -> Loc
alloc locs = size locs

putStrM :: String -> (StateT (Env, Locs a) (ExceptT Err IO)) ()
putStrM s = lift $ (lift $ putStr s);

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

evalExpr (EVar x) = do {
  (env, locs) <- get;
  case lookup x env of
    (Just l) -> return (locs ! l);
    _ -> throwError(noVarMsg x);
}

-- arithmetic expressions

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




--------------- Functions evaluating statements ---------------
evalStmt :: Stmt -> EvalMonad Val

evalStmt RetVoid = return VVoid;

evalStmt (Ret expr) = do {
  x <- evalExpr expr;
  return x;
}

evalStmt SEmpty = return VVoid

evalStmt (PrintStr sExpr) = do {
  (VString s) <- evalExpr sExpr;
  putStrM s;
  return VVoid;
}

evalStmt (PrintInt aExpr) = do {
  (VInt n) <- evalExpr aExpr;
  putStrM (show n);
  return VVoid;
}

evalStmt (StmtExpr expr) = do {
  evalExpr expr;
  return VVoid;
}

evalStmt (While bExpr stmt) = do {
  (VBool b) <- evalExpr bExpr;
  if b
    then do {
      evalStmt stmt;
      evalStmt (While bExpr stmt);
      return VVoid;
    }
    else
      return VVoid;
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
    else return VVoid;  
}

evalStmt (Ass x expr) = do {
  (env, _) <- get;
  let lMaybe = lookup x env in
    case lMaybe of
      (Just l) -> do {
        n <- evalExpr expr;
        (env1, locs1) <- get;   -- read env again in case of changes during expr evaluation
        put (env1, Data.Map.insert l n locs1);
        return VVoid;
      }
      _ -> throwError(noVarMsg x);
}

evalStmt (VarDef t (NoInit x)) = do {
  (env, locs) <- get;
  newLoc <- return (alloc locs);
  put (Data.Map.insert x newLoc env, Data.Map.insert newLoc (NotInit t) locs);
  return VVoid;
}

evalStmt (VarDef t (Init x expr)) = do {
  v <- evalExpr expr;
  (env, locs) <- get;
  newLoc <- return (alloc locs);
  put (Data.Map.insert x newLoc env, Data.Map.insert newLoc v locs);
  return VVoid;
}

evalStmt (BlockStmt (Block stmts)) = do {
  case stmts of
    [] -> return VVoid;
    (hStmt:tStmts) -> do {
      evalStmt hStmt;
      evalStmt (BlockStmt (Block tStmts));
    }
}




--------------- Functions evaluating programs ---------------

addFuncDef :: FuncDef -> EvalMonad Val
addFuncDef (FuncDef t x args block) = do {
  (env, locs) <- get;
  if member x env
    then throwError(repeatedFunMsg);
    else do {
      newLoc <- return (alloc locs);
      put (Data.Map.insert x newLoc env, Data.Map.insert newLoc (VFunc (FuncDef t x args block, (empty, empty))) locs);
      return VVoid;
    }
}



-- Sets the same global environment for every global function
-- We assume that a type of every element in Locs is VFunc 
setGlobalEnvs :: EvalMonad Val
setGlobalEnvs = do {
  (env, locs) <- get;
  let newLocs = Data.Map.map (\v -> 
        case v of
          (VFunc (f, _)) -> VFunc (f, (env, locs))
        ) locs
    in
    put (env, newLocs);
  return VVoid; 
}

evalMain :: EvalMonad Val
evalMain = do {
  (env, locs) <- get;
  case lookup (Ident "main") env of
    Nothing -> return VVoid;
    (Just l) -> case lookup l locs of
      (Just (VFunc (FuncDef Int _ [] block, _))) ->
        evalStmt (BlockStmt block);
      (Just (VFunc (FuncDef Int _ args _, _))) ->
        throwError(argsMainMsg args);
      (Just (VFunc (FuncDef t _ _ _, _))) ->
        throwError(badMainTypeMsg t);  
}

evalProg :: Prog -> EvalMonad Val
evalProg (Prog funcDefs) = do {
  case funcDefs of
    [] -> do {
      setGlobalEnvs;
      evalMain;
    }
    (hFuncDef:tFuncDef) -> do {
      addFuncDef hFuncDef;
      evalProg (Prog tFuncDef);
    }
}