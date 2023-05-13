module Exception where
import AbsGrammar

printType :: Type' a -> String
printType (Int _) = "int"
printType (Str _) = "string"
printType (Bool _) = "bool"
printType (Void _) = "void"
printType (FuncType _ retT argTs) = "function <" ++ (printType retT) ++ "> (" ++ printTypes argTs ++ ")"

printTypes :: [Type' a] -> String
printTypes types = let typeListStr = show (map (\t -> printType t) types) in
    take (-1) (drop 1 typeListStr)

errorMsg (Just (line, column)) reason = "Error around line " ++ (show line) ++ ", column " ++ (show column) ++ "\n" ++ reason
errorMsg Nothing reason = "Error at some place\n" ++ reason

wrongTypeMsg givenT expectT = "\nGiven type: " ++ (printType givenT) ++ ", expected: " ++ (printType expectT)

-- Error reasons
noVarMsg (Ident x) = x ++ " was not declared in this scope"
divZeroMsg = "Divide by zero"
repeatedFunMsg (Ident f) = "Repeated name of function in a global function definition. Function name: " ++ f
badMainTypeMsg givenT = "Wrong type of main function." ++ (wrongTypeMsg givenT (Int ()))
argsMainMsg args = "There were given some arguments in main function definition. Given arguments: " ++ (show args)
badRefArg (Ident f) t (Ident x) = "Wrong call of function " ++ f ++ ". Given argument could not be used as a reference. Parameter: " ++ (printType t) ++ " & " ++ x
wrongTypeExprOneArg exprName givenT expectT = "Wrong type in expression \"" ++ exprName ++ "\"." ++ (wrongTypeMsg givenT expectT)
wrongTypeExprTwoArg exprName whichArg givenT expectT = "Wrong type in expression \"" ++ exprName ++ "\" in the " ++ whichArg ++ " argument." ++ (wrongTypeMsg givenT expectT)
wrongTypeLambda givenT expectT = "Wrong returned type in lambda expression." ++ (wrongTypeMsg givenT expectT)
wrongArgNumb (Ident f) = "Passed wrong number of arguments in function " ++ f
wrongPassedArg (Ident f) argPos givenT expectT = "Passed wrong argument in function " ++ f ++ "in " ++ (show argPos) ++ ". argument." ++ (wrongTypeMsg givenT expectT)
notFunc (Ident x) = "Variable " ++ x ++ "is not a function"
diffRet = "Returned values of different type in a function"
wrongTypeStmtOneArg stmtName givenT expectT = "Wrong type in statement \"" ++ stmtName ++ "\"." ++ (wrongTypeMsg givenT expectT)
wrongTypeWhile givenT expectT = "Wrong type of condition expression in `while`." ++ (wrongTypeMsg givenT expectT)
wrongTypeIf givenT expectT = "Wrong type of condition expression in `if`." ++ (wrongTypeMsg givenT expectT)
wrongTypeAss x givenT expectT = let (Just (line, column)) = hasPosition expectT in
    "Wrong type of expression in assignment to var " ++ (show x) ++ " declared in line " ++ (show line) ++ ", column " ++ (show column) ++ ".\nGiven type: " ++ (printType givenT) 
wrongTypeInit givenT expectT = "Wrong type of expression in variable initialization." ++ (wrongTypeMsg givenT expectT)