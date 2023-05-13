module Exception where
import AbsGrammar

errorMsg (Just (line, column)) reason = "Error around line " ++ (show line) ++ ", column " ++ (show column) ++ "\n" ++ reason 
errorMsg Nothing reason = "Error at some place\n" ++ reason 

-- Error reasons
noVarMsg :: Ident -> String
noVarMsg x = (show x) ++ " was not declared in this scope"
divZeroMsg = "Divide by zero"
repeatedFunMsg f = "Repeated name of function in a global function definition. Function name: " ++ (show f)
badMainTypeMsg t = "The type of main function is not Int. Given type: " ++ (show t)
argsMainMsg args = "There were given some arguments in main function definition. Given arguments: " ++ (show args)
badRefArg t x = "Given argument could not be used as a reference. Parameter: " ++ (show t) ++ " " ++ (show x)
wrongTypeExprOneArg exprName givenT expectT = "Wrong type in expression \"" ++ exprName ++ "\". Given type: " ++ (show givenT) ++ ", expected: " ++ (show expectT)
wrongTypeExprTwoArg exprName whichArg givenT expectT = "Wrong type in expression \"" ++ exprName ++ "\" in the" ++ whichArg ++ " argument. Given type: " ++ (show givenT) ++ ", expected: " ++ (show expectT)
wrongTypeLambda exprStr retT expectT = "Wrong returned type in lambda expression: " ++ exprStr ++ ". Returned: " ++ (show retT) ++ ", expected: " ++ (show expectT)
wrongArgNumb f = "Passed wrong number of arguments in function " ++ (show f)
wrongPassedArg f argPos givenT expectT = "Passed wrong argument in function " ++ (show f) ++ "in " ++ (show argPos) ++ ". argument. Given type: " ++ (show givenT) ++ ", expected: " ++ (show expectT)
notFunc x = "Variable " ++ (show x) ++ "is not a function"
diffRet = "Returned values of different type in a function"
wrongTypeStmtOneArg stmtStr givenT expectT = "Wrong type in statement \"" ++ stmtStr ++ "\". Given type: " ++ (show givenT) ++ ", expected: " ++ (show expectT)
wrongTypeWhile givenT expectT = "Wrong type of condition expression in `while`. Given type: " ++ (show givenT) ++ ", expected: " ++ (show expectT)
wrongTypeIf givenT expectT = "Wrong type of condition expression in `if`. Given type: " ++ (show givenT) ++ ", expected: " ++ (show expectT)
wrongTypeAss x givenT expectT = let (Just (line, column)) = hasPosition expectT in
    "Wrong type of expression in assignment to var " ++ (show x) ++ " declared in line " ++ (show line) ++ ", column " ++ (show column) ++ ". Given type: " ++ (show givenT) ++ ", expected: " ++ (show expectT)
wrongTypeInit exprStr givenT expectT = "Wrong type of expression in variable initialization, expr: " ++ exprStr ++ ". Given type: " ++ (show givenT) ++ ", expected: " ++ (show expectT)