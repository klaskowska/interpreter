module Evaluation where

import Prelude hiding (lookup)
import Data.Map
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import AbsGrammar
import Exception

type Var = Ident
type Err = String
type Func = ([Arg], Block)
data Val = VInt Int | VBool Bool | VString String | VFunc (Func, Env) | NotInit | VVoid
type Loc = Int
type Env = Map Var Loc
type Store a = Map Loc a
type ProgState a = (Env, Store a)
-- Object which can be returned by statement
data RetObj = RVal Val | NoRet
-- Computed expression as a literal or as a variable 
-- (getting value of CompExpr won't change program's state)
data CompExpr = CVal Val | CVar Ident

-- A monad used in program components evaluation
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
evalArithm :: BNFC'Position -> (Int -> Int -> Int) -> Expr -> Expr -> Bool -> EvalMonad Val Val
evalArithm pos op aExpr1 aExpr2 isDiv = do {
  (VInt n1) <- evalExpr aExpr1;
  (VInt n2) <- evalExpr aExpr2;
  if isDiv && n2 == 0
    then throwError (errorMsg pos divZeroMsg);
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

-- Computes list of expressions; 
-- expression can evaluate to value or stay as variable
compMultiExpr :: [Expr] -> [CompExpr] -> EvalMonad Val [CompExpr]
compMultiExpr [] vals = return (reverse vals)
compMultiExpr (exprH:exprT) vals = do {
  case exprH of
    (EVar _ x) -> compMultiExpr exprT ((CVar x):vals);
    otherwise -> do {
      v <- evalExpr exprH;
      compMultiExpr exprT ((CVal v):vals);
    }
}

-- Returns local program state for a function (sets parameters to given arguments) 
setParams :: BNFC'Position -> Ident -> [Arg] -> [CompExpr] -> (ProgState Val) -> EvalMonad Val (ProgState Val)
setParams _ _ [] [] state = return state;
setParams pos f (argH:argT) (compH:compT) (env, store) = do {
  case argH of
    (ArgVal _ _ x) -> do {
      newLoc <- return (alloc store);
      case compH of
        (CVal val) -> setParams pos f argT compT (Data.Map.insert x newLoc env, Data.Map.insert newLoc val store);
        (CVar var) -> do {
          val <- evalExpr (EVar pos var);
          setParams pos f argT compT (Data.Map.insert x newLoc env, Data.Map.insert newLoc val store);
        }
    }
    (ArgRef _ t x) -> do {
      case compH of
        (CVal _) -> throwError (errorMsg pos (badRefArg f t x));
        (CVar var) -> do {
          (globEnv, _) <- get; 
          let (Just loc) = lookup var globEnv in
            setParams pos f argT compT (Data.Map.insert x loc env, store);
        }
    }
}

-- Evaluates statement, but leaves environment whitout variables declared in this statement
evalScope :: Stmt -> EvalMonad Val RetObj
evalScope stmt = do {
  (globalEnv, _) <- get;
  res <- evalStmt stmt;
  (newEnv, newStore) <- get;
  put (globalEnv, newStore);
  return res;
}

--------------- Function evaluating expressions ---------------
evalExpr :: Expr -> EvalMonad Val Val

evalExpr (EVar pos x) = do {
  (env, store) <- get;
  let (Just l) = lookup x env in
    let v = store ! l in
      case v of
        NotInit -> throwError (errorMsg pos (notInit x));
        otherwise -> return v;
}

evalExpr (ECallFunc pos f args) = do {
  (VFunc ((argsDef, block), fEnv)) <- evalExpr (EVar pos f);
  vals <- compMultiExpr args [];
  (globEnv, store) <- get;
  localState <- setParams pos f argsDef vals (fEnv, store);
  put (localState);
  (RVal v) <- evalStmt (BlockStmt pos block);
  (_, newStore) <- get;
  put (globEnv, newStore);
  return v;
}

evalExpr (ELambda _ args _ block) = do {
  (env, _) <- get;
  return (VFunc ((args, block), env));
}

-- arithmetic expressions

evalExpr (EInt _ n) = return (VInt (fromIntegral n))

evalExpr (Neg _ aExpr) = do {
  (VInt n) <- evalExpr aExpr;
  return (VInt (negate n));
}

evalExpr (EMul pos aExpr1 (Times _) aExpr2) = evalArithm pos (*) aExpr1 aExpr2 False

evalExpr (EMul pos aExpr1 (Div _) aExpr2) = evalArithm pos div aExpr1 aExpr2 True

evalExpr (EMul pos aExpr1 (Mod _) aExpr2) = evalArithm pos mod aExpr1 aExpr2 True

evalExpr (EAdd pos aExpr1 (Plus _) aExpr2) = evalArithm pos (+) aExpr1 aExpr2 False

evalExpr (EAdd pos aExpr1 (Minus _) aExpr2) = evalArithm pos (-) aExpr1 aExpr2 False

-- boolean expressions

evalExpr (ETrue _) = return (VBool True)

evalExpr (EFalse _) = return (VBool False)

evalExpr (Not _ bExpr) = do {
  (VBool b) <- evalExpr bExpr;
  return (VBool (not b));
}

evalExpr (ERel _ aExpr1 (LTH _) aExpr2) = evalRel (<) aExpr1 aExpr2

evalExpr (ERel _ aExpr1 (LE _) aExpr2) = evalRel (<=) aExpr1 aExpr2

evalExpr (ERel _ aExpr1 (GTH _) aExpr2) = evalRel (>) aExpr1 aExpr2

evalExpr (ERel _ aExpr1 (GE _) aExpr2) = evalRel (>=) aExpr1 aExpr2

evalExpr (ERel _ aExpr1 (EQU _) aExpr2) = evalRel (==) aExpr1 aExpr2

evalExpr (ERel _ aExpr1 (NE _) aExpr2) = evalRel (/=) aExpr1 aExpr2

evalExpr (EAnd _ bExpr1 bExpr2) = evalBool (&&) bExpr1 bExpr2

evalExpr (EOr _ bExpr1 bExpr2) = evalBool (||) bExpr1 bExpr2


-- string expressions

evalExpr (EString _ s) = return (VString s)




--------------- Function evaluating statements ---------------
evalStmt :: Stmt -> EvalMonad Val RetObj

evalStmt (RetVoid _) = return (RVal VVoid)

evalStmt (Ret _ expr) = do {
  x <- evalExpr expr;
  return (RVal x);
}

evalStmt (SEmpty _) = return NoRet

evalStmt (PrintStr _ sExpr) = do {
  (VString s) <- evalExpr sExpr;
  putStrM s;
  return NoRet;
}

evalStmt (PrintInt _ aExpr) = do {
  (VInt n) <- evalExpr aExpr;
  putStrM (show n);
  return NoRet;
}

evalStmt (StmtExpr _ expr) = do {
  evalExpr expr;
  return NoRet;
}

evalStmt (CallFunc pos f args) = do {
  evalExpr (ECallFunc pos f args);
  return NoRet;
}

evalStmt (While pos bExpr stmt) = do {
  (VBool b) <- evalExpr bExpr;
  if b
    then do {
      retObj <- evalScope stmt;
      case retObj of
        NoRet -> evalStmt (While pos bExpr stmt);
        otherwise -> return retObj;
    }
    else
      return NoRet;
}

evalStmt (IfElse _ bExpr stmt1 stmt2) = do {
  (VBool b) <- evalExpr bExpr;
  if b
    then evalScope stmt1;
    else evalScope stmt2;
} 

evalStmt (If _ bExpr stmt) = do {
  (VBool b) <- evalExpr bExpr;
  if b
    then evalScope stmt;
    else return NoRet;  
}

evalStmt (Ass _ x expr) = do {
  (env, _) <- get;
  let (Just l) = lookup x env in
    do {
      n <- evalExpr expr;
      (env1, store1) <- get;   -- read env again in case of changes during expr evaluation
      put (env1, Data.Map.insert l n store1);
      return NoRet;
    }
}

evalStmt (VarDef _ t (NoInit _ x)) = do {
  (env, store) <- get;
  newLoc <- return (alloc store);
  put (Data.Map.insert x newLoc env, Data.Map.insert newLoc NotInit store);
  return NoRet;
}

evalStmt (VarDef _ _ (Init _ x expr)) = do {
  v <- evalExpr expr;
  (env, store) <- get;
  newLoc <- return (alloc store);
  put (Data.Map.insert x newLoc env, Data.Map.insert newLoc v store);
  return NoRet;
}

evalStmt (BlockStmt _ (Block _ [])) = return NoRet;
evalStmt (BlockStmt posStmt (Block pos (stmtH:stmtT))) = do {
  retObj <- evalStmt stmtH;
  case retObj of
    NoRet -> evalStmt (BlockStmt posStmt (Block pos stmtT));
    otherwise -> return retObj;
}


--------------- Functions evaluating programs ---------------

-- TypeChecker potentially checks if main function has a proper signature
addFuncDef :: FuncDef -> EvalMonad Val ()
addFuncDef (FuncDef pos t f args block) = do {
  (env, store) <- get;
  if member f env
    then throwError (errorMsg pos (repeatedFunMsg f));
    else do {
      newLoc <- return (alloc store);
      let newEnv = Data.Map.insert f newLoc env in
        let newStore = Data.Map.insert newLoc (VFunc ((args, block), newEnv)) store in
          put (newEnv, newStore);
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
}

evalMain :: EvalMonad Val RetObj
evalMain = do {
  (env, store) <- get;
  case lookup (Ident "main") env of
    Nothing -> return NoRet;
    (Just l) -> 
      let (Just (VFunc ((_, block), _))) = lookup l store in
        evalStmt (BlockStmt Nothing block);
}

evalProg :: Prog -> EvalMonad Val RetObj
evalProg (Prog _ funcDefs) = do {
  case funcDefs of
    [] -> do {
      setGlobalEnvs;
      evalMain;
    }
    (hFuncDef:tFuncDef) -> do {
      addFuncDef hFuncDef;
      evalProg (Prog Nothing tFuncDef);
    }
}