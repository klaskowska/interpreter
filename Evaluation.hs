-- TODO
-- 1. Generalize arithmetic operations
-- 3. Remeber to check if NotInit matches assinging in type checker
-- 5. Think about checking if return has been used (for now there is a haskell error when nothing is returned)
-- 6. Line numbers in error messages
-- 7. Free space in Store

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
badRefArg t x = "Given argument could not be used as a reference. Parameter: " ++ (show t) ++ " " ++ (show x)


-- Types used in interpreter
type Func = ([Arg], Block)
data Val = VInt Int | VBool Bool | VString String | VFunc (Func, Env) | NotInit Type | VVoid
-- Object which can be returned by statement
data RetObj = RVal Val | NoRet
-- Computed expression as a literal or as a variable 
-- (getting value of CompExpr won't change program's state)
data CompExpr = CVal Val | CVar Ident
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

----------------------- Helper functions -----------------------
alloc :: Store a -> Loc
alloc store = size store

putStrM :: String -> (StateT (ProgState a) (ExceptT Err IO)) ()
putStrM s = lift $ (lift $ putStr s);

-- if `isDiv` and `aExpr2` evaluates to 0 then throws exception
evalArithm :: (Int -> Int -> Int) -> Expr -> Expr -> Bool -> EvalMonad Val Val
evalArithm op aExpr1 aExpr2 isDiv = do {
  (VInt n1) <- evalExpr aExpr1;
  (VInt n2) <- evalExpr aExpr2;
  if isDiv && n2 == 0
    then throwError(divZeroMsg);
    else return (VInt (op n1 n2));
}

evalRel :: (Int -> Int -> Bool) -> Expr -> Expr -> EvalMonad Val Val
evalRel op aExpr1 aExpr2 = do {
  (VInt n1) <- evalExpr aExpr1;
  (VInt n2) <- evalExpr aExpr2;
  return (VBool (op n1 n2));
}

evalBool :: (Bool -> Bool -> Bool) -> Expr -> Expr -> EvalMonad Val Val
evalBool op bExpr1 bExpr2 = do {
  (VBool b1) <- evalExpr bExpr1;
  (VBool b2) <- evalExpr bExpr2;
  return (VBool (op b1 b2));
}

-- TODO: check if this function is used in more than one place
initVar :: Ident -> Expr -> EvalMonad Val (Env, Store Val)
initVar x expr = do {
  v <- evalExpr expr;
  (env, store) <- get;
  newLoc <- return (alloc store);
  return (Data.Map.insert x newLoc env, Data.Map.insert newLoc v store);
}

compMultiExpr :: [Expr] -> [CompExpr] -> EvalMonad Val [CompExpr]
compMultiExpr [] vals = return (reverse vals)
compMultiExpr (exprH:exprT) vals = do {
  case exprH of
    (EVar x) -> compMultiExpr exprT ((CVar x):vals);
    otherwise -> do {
      v <- evalExpr exprH;
      compMultiExpr exprT ((CVal v):vals);
    }
}

-- Returns local program state for a function (sets parameters to given arguments) 
setParams :: [Arg] -> [CompExpr] -> (ProgState Val) -> EvalMonad Val (ProgState Val)
setParams [] [] state = return state;
setParams (argH:argT) (compH:compT) (env, store) = do {
  case argH of
    (ArgVal _ x) -> do {
      newLoc <- return (alloc store);
      case compH of
        (CVal val) -> setParams argT compT (Data.Map.insert x newLoc env, Data.Map.insert newLoc val store);
        (CVar var) -> do {
          val <- evalExpr (EVar var);
          setParams argT compT (Data.Map.insert x newLoc env, Data.Map.insert newLoc val store);
        }
    }
    (ArgRef t x) -> do {
      case compH of
        (CVal _) -> throwError (badRefArg t x);
        (CVar var) -> do {
          (globEnv, _) <- get; 
          let (Just loc) = lookup var globEnv in
            setParams argT compT (Data.Map.insert x loc env, store);
        }
    }
}

--------------- Functions evaluating expressions ---------------

evalExpr :: Expr -> EvalMonad Val Val

evalExpr (EVar x) = do {
  (env, store) <- get;
  case lookup x env of
    (Just l) -> return (store ! l);
    _ -> throwError(noVarMsg x);
}

evalExpr (ECallFunc f args) = do {
  (VFunc ((argsDef, block), fEnv)) <- evalExpr (EVar f);
  vals <- compMultiExpr args [];
  (globEnv, store) <- get;
  localState <- setParams argsDef vals (fEnv, store);
  put (localState);
  (RVal v) <- evalStmt (BlockStmt block);
  (_, newStore) <- get;
  put (globEnv, newStore);
  return v;
}

evalExpr (ELambda args t block) = do {
  (env, store) <- get;
  return (VFunc ((args, block), env));
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
evalStmt :: Stmt -> EvalMonad Val RetObj

evalStmt RetVoid = return (RVal VVoid)

evalStmt (Ret expr) = do {
  x <- evalExpr expr;
  return (RVal x);
}

evalStmt SEmpty = return NoRet

evalStmt (PrintStr sExpr) = do {
  (VString s) <- evalExpr sExpr;
  putStrM s;
  return NoRet;
}

evalStmt (PrintInt aExpr) = do {
  (VInt n) <- evalExpr aExpr;
  putStrM (show n);
  return NoRet;
}

evalStmt (StmtExpr expr) = do {
  evalExpr expr;
  return NoRet;
}

evalStmt (CallFunc f args) = do {
  evalExpr (ECallFunc f args);
  return NoRet;
}

evalStmt (While bExpr stmt) = do {
  (VBool b) <- evalExpr bExpr;
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

evalStmt (BlockStmt (Block [])) = return NoRet;
evalStmt (BlockStmt (Block (stmtH:stmtT))) = do {
  retObj <- evalStmt stmtH;
  case retObj of
    NoRet -> evalStmt (BlockStmt (Block stmtT));
    otherwise -> return retObj;
}




--------------- Functions evaluating programs ---------------

-- TypeChecker potentially checks if main function has a proper signature
addFuncDef :: FuncDef -> EvalMonad Val ()
addFuncDef (FuncDef t f args block) = do {
  (env, store) <- get;
  if member f env
    then throwError(repeatedFunMsg);
    else do {
      newLoc <- return (alloc store);
      let newEnv = Data.Map.insert f newLoc env in
        let newStore = Data.Map.insert newLoc (VFunc ((args, block), newEnv)) store in
          put (newEnv, newStore);
      return ();
    }
}

-- Sets the same global environment for every global function
-- We assume that a type of every element in Store is VFunc 
setGlobalEnvs :: EvalMonad Val ()
setGlobalEnvs = do {
  (env, store) <- get;
  let newStore = Data.Map.map (\v -> 
        case v of
          (VFunc (f, _)) -> VFunc (f, env)
        ) store
    in
    put (env, newStore);
  return (); 
}

evalMain :: EvalMonad Val RetObj
evalMain = do {
  (env, store) <- get;
  case lookup (Ident "main") env of
    Nothing -> return NoRet;
    (Just l) -> 
      let (Just (VFunc ((_, block), _))) = lookup l store in
        evalStmt (BlockStmt block);
{-
-- may be useful for typeChecker      
      case lookup l store of
      (Just (VFunc (FuncDef Int _ [] block, _))) ->
        evalStmt (BlockStmt block);
      (Just (VFunc (FuncDef Int _ args _, _))) ->
        throwError(argsMainMsg args);
      (Just (VFunc (FuncDef t _ _ _, _))) ->
        throwError(badMainTypeMsg t);  
-}
}

evalProg :: Prog -> EvalMonad Val RetObj
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