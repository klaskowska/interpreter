module Exception where
import AbsGrammar

-- Error messages
noVarMsg :: Ident -> String
noVarMsg x = "Unknown variable " ++ (show x) ++ " at some place"
divZeroMsg = "Divide by zero"
repeatedFunMsg = "Repeated name of function in a global function definition"
badMainTypeMsg t = "The type of main function is not Int. Given type: " ++ (show t)
argsMainMsg args = "There were given some arguments in main function definition. Given arguments: " ++ (show args)
badRefArg t x = "Given argument could not be used as a reference. Parameter: " ++ (show t) ++ " " ++ (show x)
wrongTypeExprOneArg exprStr givenT expectT = "Wrong type in expression \"" ++ exprStr ++ "\". Given type: " ++ (show givenT) ++ ", expected: " ++ (show expectT)
wrongTypeExprTwoArg exprStr whichArg givenT expectT = "Wrong type in expression \"" ++ exprStr ++ "\" in the" ++ whichArg ++ " argument. Given type: " ++ (show givenT) ++ ", expected: " ++ (show expectT)