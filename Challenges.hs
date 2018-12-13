-- COMP2209 Coursework 2, University of Southampton 2018
-- DUMMY FILE FOR YOU TO EDIT AND ADD YOUR OWN IMPLEMENTATIONS
-- NOTE THAT NO THIRD PARTY MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION TYPE SIGNATURES NOR TYPE DEFINITIONS 
-- This module statement makes public only the specified functions and types
-- DO NOT CHANGE THIS LIST OF EXPORTED FUNCTIONS AND TYPES
module Challenges (convertLet, prettyPrint, parseLet, countReds, compileArith,
    Expr(App, Let, Var), LamExpr(LamApp, LamAbs, LamVar)) where

import Data.Char
import Data.List
import Parsing

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

-- convert a let expression to lambda expression
convertLet :: Expr -> LamExpr
convertLet e
    | (App e1 e2) <- e                           = LamApp (convertLet e1) (convertLet e2)
    | (Let x e1@(Let _ _ _) e2@(Let _ _ _)) <- e = LamApp (LamAbs (x!!0) (convertLet e1)) (prefixAbsChain (tail x) (convertLet e2))
    | (Let x e1@(Let _ _ _) e2) <- e             = LamApp (LamAbs (x!!0) (listToApp (parseExprToList e2))) (prefixAbsChain (tail x) (convertLet e1))
    | (Let x e1 e2@(Let _ _ _)) <- e             = LamApp (LamAbs (x!!0) (convertLet e2)) (makeAbsChain (tail x) (parseExprToList e1))
    | (Let x e1 e2) <- e                         = LamApp (LamAbs (x!!0) (listToApp (parseExprToList e2))) (makeAbsChain (tail x) (parseExprToList e1))
    
--Parses App and Var expressions.
parseExprToList :: Expr -> [Int]
parseExprToList (App a b) = (parseExprToList a) ++ (parseExprToList b)
parseExprToList (Var x) = [x]

--Converts a list of integers into a LamApp LamVar chain
listToApp :: [Int] -> LamExpr
listToApp (x:xs)
    | length (x:xs) == 1 = LamVar x
    | length (x:xs) > 1 = LamApp (LamVar x) (listToApp xs)

--Creates the chain of lambdas for let expressions with multiple variables before the equals.
makeAbsChain :: [Int] -> [Int] -> LamExpr
makeAbsChain [] a = (listToApp a)
makeAbsChain (x:xs) a = LamAbs x (makeAbsChain xs a)

prefixAbsChain :: [Int] -> LamExpr -> LamExpr
prefixAbsChain [] a = (a)
prefixAbsChain (x:xs) a = LamAbs x (prefixAbsChain xs a)

-- Challenge 2
-- pretty print a let expression by converting it to a string
prettyPrint :: Expr -> String
prettyPrint e
    | (Let x e1 e2) <- e = "let " ++ parseListToString x ++ " = " ++ prettyPrint e1 ++ " in " ++ prettyPrint e2
    | (App x y) <- e     = parseExprToString (App x y)
    | (Var x) <- e       = "x" ++ (show x)

--Converts a list of variables before the equals in the let expressio into a string representation.
parseListToString :: [Int] -> String
parseListToString (x:xs)
    | length (x:xs) == 0 = ""
    | length (x:xs) == 1 = "x" ++ (show x)
    | length (x:xs) > 1  = "x" ++ (show x) ++ " " ++ parseListToString (xs)

parseExprToString :: Expr -> String
parseExprToString e
    | (App e1@(App _ _) e2@(App _ _)) <- e     = parseExprToString e1 ++ " (" ++ parseExprToString e2 ++ ")"
    | (App e1@(Var _) e2@(App _ _)) <- e       = parseExprToString e1 ++ " (" ++ parseExprToString e2 ++ ")"
    | (App e1@(App _ _) e2@(Let _ _ _)) <- e   = parseExprToString e1 ++ " (" ++ prettyPrint e2 ++ ")"
    | (App e1@(Var _) e2@(Let _ _ _)) <- e     = parseExprToString e1 ++ " (" ++ prettyPrint e2 ++ ")"
    | (App e1@(Let _ _ _) e2@(App _ _)) <- e   = "(" ++ prettyPrint e1 ++ ") (" ++ parseExprToString e2 ++ ")"
    | (App e1@(Let _ _ _) e2@(Let _ _ _)) <- e = "(" ++ prettyPrint e1 ++ ") (" ++ prettyPrint e2 ++ ")"
    | (App e1@(App _ _) e2@(Var _)) <- e       = parseExprToString e1 ++ " " ++ parseExprToString e2
    | (App e1@(Var _) e2@(Var _)) <- e         = parseExprToString e1 ++ " " ++ parseExprToString e2
    | (App e1@(Let _ _ _) e2@(Var _)) <- e     = "(" ++ prettyPrint e1 ++ ") " ++ parseExprToString e2
    | (Var x) <- e                             = "x" ++ (show x)

-- Challenge 3
-- parse a let expression
parseLet :: String -> Maybe Expr
parseLet s
    | parse parseStrToLet s == [] && parse parseStrToApp s == [] && parse parseStrToVar s == [] = Nothing
    | otherwise = Just (fst $ head $ parse (parseStrToApp <|> parseStrToLet <|> parseStrToVar) s)

--Parser checks let expressions follow correct syntax.
parseStrToLet :: Parser Expr
parseStrToLet = do
    _ <- symbol "let"
    ints <- parseStrToIntList
    _ <- symbol "="
    --Expression can either be App, Var or another Let.
    e1 <- parseStrToApp <|> parseStrToVar <|> parseStrToLet
    _ <- symbol "in"
    e2 <- parseStrToApp <|> parseStrToVar <|> parseStrToLet
    return (Let ints e1 e2)

--Parser checks for nested app expressions recursively following correct syntax.
parseStrToApp :: Parser Expr
parseStrToApp = do
    e1 <- rmBrackets <|> parseStrToVar
    e2 <- rmBrackets <|> parseStrToVar
    ex <- many (parseStrToApp <|> parseStrToVar)
    formatAppExpr e1 e2 ex

--Removes brackets surrounding a statement
rmBrackets :: Parser Expr
rmBrackets = do
    _ <- space
    _ <- char '('
    e <- parseStrToApp <|> parseStrToLet <|> parseStrToVar
    _ <- char ')'
    _ <- space
    return e

--Parser checks a var expression following correct syntax.
parseStrToVar :: Parser Expr
parseStrToVar = do
    x <- parseStrToInt
    return (Var x)

--Formats output App Expression according to parsed data.
formatAppExpr :: Expr -> Expr -> [Expr] -> Parser Expr
formatAppExpr e1 e2 ex
    | length ex == 0 = return (App e1 e2)
    | otherwise      = return (App (App e1 e2) (ex !! 0))

--Retreives integer from a string representation
parseStrToInt :: Parser Int
parseStrToInt = do
    _ <- char 'x'
    intVar <- nat
    _ <- space
    return (intVar)

--Repeatedly applies parseStrToInt 
parseStrToIntList :: Parser [Int]
parseStrToIntList = do
    intList <- many parseStrToInt
    if (length intList == 0) then return ([]) else do
        _ <- space
        tail <- parseStrToIntList
        return (intList ++ tail)

-- Challenge 4
-- count reductions using two different strategies 
countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
-- replace the definition below with your solution
countReds e limit = (left, right)
    where 
        left  = runTimes limit (leftReduce) e 0
        right = runTimes limit (rightReduce) e 0

--Determines whether reductions with a given reduction function on an expression complete in a maximum number of reductions, 
--returning the number of reductions required if they do, or Nothing if they do not.
runTimes :: Int -> (LamExpr -> LamExpr) -> LamExpr -> Int -> Maybe Int
runTimes 0 f e count
    | (isComplete f e) = Just (count)
    | otherwise = Nothing
runTimes n f e count
    | (isComplete f e) = Just (count)
    | otherwise = runTimes (n-1) f (f e) (count + 1)

--Determines if all reductions are complete on an expression by attempting to reduce once more.
--If this changes the value, it's not complete. If it does, it's already reduced fully.
isComplete :: (LamExpr -> LamExpr) -> LamExpr -> Bool
isComplete f e 
    | e == e' = True
    | otherwise = False
    where e' = (f e)

--Leftmost-Inntermost 1-Step reduction
leftReduce :: LamExpr -> LamExpr
leftReduce (LamVar x) = LamVar x
leftReduce (LamAbs x e) = LamAbs x (leftReduce e)
leftReduce (LamApp e1@(LamAbs x e) e2)
    | e1 == e1' = subst e x e2
    | otherwise = LamApp e1' e2
    where e1' = leftReduce e1

leftReduce (LamApp e1 e2)
    | e1 == e1' = LamApp e1 (leftReduce e2)
    | otherwise = LamApp e1' e2
    where e1' = leftReduce e1

--Rightmost-Innermost 1-step reuction
rightReduce :: LamExpr -> LamExpr
rightReduce (LamVar x) = LamVar x
rightReduce (LamAbs x e) = LamAbs x (leftReduce e)
rightReduce (LamApp e1@(LamAbs x e) e2)
    | e2 == e2' = subst e x e2
    | otherwise = LamApp e1 e2'
    where e2' = rightReduce e2

rightReduce (LamApp e1 e2)
    | e2 == e2' = LamApp (rightReduce e1) e2
    | otherwise = LamApp e1 e2'
    where e2' = rightReduce e2

--This is adapted from the code given to us in Lecture 13's slides.
--Modified to work with LamExpr instead, and to use Int rather than String
subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e |
 x /= y && not (free x e) = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e |
    x /=y && (free x e) = let x' = rename x y in
            subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e | x == y = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e) 

--Adapted from Lecture 13's slides
free :: Int -> LamExpr -> Bool
free x (LamVar y) = x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)

--'Renames' to the largest of the two expression +1
rename :: Int -> Int -> Int
rename x y = (max x y) + 1

-- Challenge 5
-- compile an arithmetic expression into a lambda calculus equivalent

--Data type to represent BNF of ArithmeticExpression. The input string is encoded as an AritExpr by the parser.
data AritExpr = Sect (AritExpr) | SectVal (AritExpr) (AritExpr) | Natural (Int) | Add (AritExpr) (AritExpr) | BrackVal (AritExpr) deriving (Show, Eq)

compileArith :: String -> Maybe LamExpr
compileArith s
    | validStringCheck s = Just (genExpr $ parseArith s)
    | otherwise = Nothing

validStringCheck :: String -> Bool
validStringCheck s
    | "++" `isInfixOf` s = False
    | length s < 0 = False
    | otherwise = True

genExpr :: Maybe AritExpr -> LamExpr
genExpr e
    | Just (Natural x) <- e              = digitToExpr x
    | Just (Sect (Natural x)) <- e       = LamApp (digitToExpr x) (succExpr)
    | Just (BrackVal (Natural x)) <- e   = digitToExpr x
    | Just (BrackVal e1) <- e            = genExpr (Just e1)
    | Just (Add e1 e2) <- e              = LamApp (LamApp (genExpr (Just e1)) (plusExpr)) (genExpr (Just e2))
    | Just (SectVal (Natural x) e2) <- e = LamApp (genExpr (Just (Sect (Natural x)))) (genExpr (Just e2))
    | Just (SectVal e1 e2) <- e          = LamApp (genExpr (Just e1)) (genExpr (Just e2))

------ All parsing stuff (from String into AritExpt)
parseArith :: String -> Maybe AritExpr
parseArith s
    | parse (valueStrParser <|> parseStrToSect) s == [] = Nothing
    | unreadString == "" = Just (aritExpr)
    | otherwise = Nothing
    where 
        parsedArithExpr = parse (valueStrParser <|> parseStrToSect ) s
        (aritExpr, unreadString) = head parsedArithExpr

valueStrParser :: Parser AritExpr
valueStrParser = do
    e1 <- (parseStrToSectVal <|> parseStrToValVal  <|> parseStrToBrackVal <|> parseStrToNat)
    return e1

--Parses Value = Natural
parseStrToNat :: Parser AritExpr
parseStrToNat = do
    val <- nat
    return (Natural (val))

--Parses Value = Value "+" Value
parseStrToValVal :: Parser AritExpr
parseStrToValVal = do
    val1 <- (parseStrToNat <|> parseStrToBrackVal <|> parseStrToSectVal)
    _ <- char '+'
    val2 <- (parseStrToNat <|> parseStrToBrackVal <|> parseStrToSectVal)
    return (Add (val1) (val2))

--Parses bracketed values ( Value )
parseStrToBrackVal :: Parser AritExpr
parseStrToBrackVal = do
    _ <- char '('
    val <- valueStrParser
    _ <- char ')'
    return (BrackVal (val))

--Parses Section definition for Value = Section Value
parseStrToSectVal :: Parser AritExpr
parseStrToSectVal = do
    _ <- char '('
    _ <- char '+'
    sect <- valueStrParser
    _ <- char ')'
    value <- valueStrParser
    return (SectVal (sect) (value))
    
--Parses Section definition for ArithExpr = Section
parseStrToSect :: Parser AritExpr
parseStrToSect = do
    _ <- char '('
    _ <- char '+'
    sect <- valueStrParser
    _ <- char ')'
    return (Sect (sect))

--0 = (\x -> (\y -> y))
--1 = (\x -> (\y -> x y))
--2 = (\x -> (\y -> x (x y)))
--where x has been replaced by 1 and y has been replaced by 2.
digitToExpr :: Int -> LamExpr
digitToExpr n
    | n > 0  = (LamAbs 1 (LamAbs 2 (recurseDigit n)))
    | n == 1 = (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2))))
    | n == 0 = (LamAbs 1 (LamAbs 2 (LamVar 2)))
    
--Does the recursive bit of encoding a digit.
recurseDigit :: Int -> LamExpr
recurseDigit n
    | n > 0 = (LamApp (LamVar 1) (recurseDigit (n-1)))
    | otherwise = (LamVar 2)

--The Add Expression appended to the end of a digit expression to change it from 1 to (+1).
succExpr = (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))
--The Plus expression appended between expression in a Value + Value BNF.
plusExpr = (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamApp (LamApp (LamVar 1) (LamVar 3)) (LamApp (LamApp (LamVar 2) (LamVar 3)) (LamVar 4)))))))




---- My own tests

testsExtn :: [(String, [(String, Bool)])]
testsExtn = 
  [ 
  ("Challenge 1",
    [ 
        ("Test 1: convertLet (Let [1,2,3,4,5] (Var 2) (Var 1))", 
        convertLet (Let [1,2,3,4,5] (Var 2) (Var 1)) == LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamAbs 5 (LamVar 2)))))
      ),
      ("Test 2: convertLet (App (Let [1,2,3] (Var 1) (Var 3)) (App (Let [1] (Var 1) (Var 2)) (Let [1,2] (Var 3) (Var 4))))",
        convertLet (App (Let [1,2,3] (Var 1) (Var 3)) (App (Let [1] (Var 1) (Var 2)) (Let [1,2] (Var 3) (Var 4)))) == LamApp (LamApp (LamAbs 1 (LamVar 3)) (LamAbs 2 (LamAbs 3 (LamVar 1)))) (LamApp (LamApp (LamAbs 1 (LamVar 2)) (LamVar 1)) (LamApp (LamAbs 1 (LamVar 4)) (LamAbs 2 (LamVar 3))))
      ),
      ("Test 3: convertLet (Let [1,2,3] (Let [3] (Var 4) (App (Var 1) (Var 3))) (Let [3] (Var 4) (App (Var 1) (Var 3))))",
        convertLet (Let [1,2,3] (Let [3] (Var 4) (App (Var 1) (Var 3))) (Let [3] (Var 4) (App (Var 1) (Var 3)))) == LamApp (LamAbs 1 (LamApp (LamAbs 3 (LamApp (LamVar 1) (LamVar 3))) (LamVar 4))) (LamAbs 2 (LamAbs 3 (LamApp (LamAbs 3 (LamApp (LamVar 1) (LamVar 3))) (LamVar 4))))
      ),
      ("Test 4: convertLet (Let [1] (Let [3] (Var 4) (App (Var 1) (Var 3))) (Let [3] (Var 4) (App (Var 1) (Var 3))))",
       convertLet (Let [1] (Let [3] (Var 4) (App (Var 1) (Var 3))) (Let [3] (Var 4) (App (Var 1) (Var 3)))) == LamApp (LamAbs 1 (LamApp (LamAbs 3 (LamApp (LamVar 1) (LamVar 3))) (LamVar 4))) (LamApp (LamAbs 3 (LamApp (LamVar 1) (LamVar 3))) (LamVar 4))
      )
    ]
  ),
  ("Challenge 2",
    [ ("Test 1: prettyPrint (App (App (App (App (Var 3) (Var 2)) (App (Var 1) (Var 1))) (Var 7)) (Var 3))",
       prettyPrint (App (App (App (App (Var 3) (Var 2)) (App (Var 1) (Var 1))) (Var 7)) (Var 3)) == "x3 x2 (x1 x1) x7 x3"
      ),
      ("Test 2: prettyPrint (App (Let [1,2,3] (Var 1) (Var 3)) (App (Let [1] (Var 1) (Var 2)) (Let [1,2] (Var 3) (Var 4))))",
      prettyPrint (App (Let [1,2,3] (Var 1) (Var 3)) (App (Let [1] (Var 1) (Var 2)) (Let [1,2] (Var 3) (Var 4)))) == "(let x1 x2 x3 = x1 in x3) ((let x1 = x1 in x2) (let x1 x2 = x3 in x4))"
      )
    ]
  ), 
  ("Challenge 3",
    [ ("Test 1: parseLet (x1) x2 x3",
        (parseLet "(x1) x2 x3") == Just (App (App (Var 1) (Var 2)) (Var 3))
      ),
      ("Test 2: parseLet let x1 x2 = let x1 = x2 in x1 in x4 x5",
        (parseLet "let x1 x2 = let x1 = x2 in x1 in x4 x5") == Just (Let [1,2] (Let [1] (Var 2) (Var 1)) (App (Var 4) (Var 5)))
      ),
      ("Test 3: parseLet let x1 x2 = let x1 = x2 in x1 in let x3 = x6 in x7 x5",
        (parseLet "let x1 x2 = let x1 = x2 in x1 in let x3 = x6 in x7 x5") == Just (Let [1,2] (Let [1] (Var 2) (Var 1)) (Let [3] (Var 6) (App (Var 7) (Var 5))))
      ),
      ("Test 4: parseLet let x1 = 7 in x1 x2 x3",
        (parseLet "let x1 = 7 in x1 x2 x3") == Nothing
      ),
      ("Test 5: parseLet let x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 = x1 in x1 x2 x3",
        (parseLet "let x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 = x1 in x1 x2 x3") == Just (Let [1,2,3,4,5,6,7,8,9,10,11,12,13,14] (Var 1) (App (App (Var 1) (Var 2)) (Var 3)))
      )
    ]
  ), 
  ("Challenge 4",
    [ ("Test 1: countReds \\x1 (\\x2 -> x2) 0 = (Just 0, Just 0)", 
        countReds (LamApp lamExpr1 (LamApp lamExpr2 lamExpr3)) 100 == (Just 5,Just 9)
      ),
      ("Test 2: countReds (\\x1 -> x1)(\\x2 -> \\x2) 1 = (Just 1, Just 1)",
        countReds (LamApp lamExpr1 (LamApp lamExpr2 lamExpr3)) 6 == (Just 5,Nothing)
      ),
      ("Test 3: countReds (LamApp lamExpr1 (LamApp (lamExpr2) (LamApp (lamExpr3) (lamExpr6))))",
      countReds (LamApp lamExpr1 (LamApp (lamExpr2) (LamApp (lamExpr3) (lamExpr6)))) 15 == (Just 5,Just 13)
      )
    ]
  ), 
  ("Challenge 5",
    [ ("Test 1: compileArith (1+1)", 
       (compileArith "(1+1)") == Just (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))) (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamApp (LamApp (LamVar 1) (LamVar 3)) (LamApp (LamApp (LamVar 2) (LamVar 3)) (LamVar 4)))))))) (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))))
      ),
      ("Test 2: compileArith (+1)(+1)(1+1)",
       (compileArith "(+1)(+1)(1+1)") == Just (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))) (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))) (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))) (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamApp (LamApp (LamVar 1) (LamVar 3)) (LamApp (LamApp (LamVar 2) (LamVar 3)) (LamVar 4)))))))) (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))))))
      ),
      ("Test 3: compileArith 12",
       (compileArith "12") == Just (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamApp (LamVar 1) (LamApp (LamVar 1) (LamApp (LamVar 1) (LamApp (LamVar 1) (LamApp (LamVar 1) (LamApp (LamVar 1) (LamApp (LamVar 1) (LamApp (LamVar 1) (LamApp (LamVar 1) (LamApp (LamVar 1) (LamApp (LamVar 1) (LamVar 2)))))))))))))))
      )
    ]
  )
  ]
  
lamExpr1 = LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1))
lamExpr2 = LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamApp (LamAbs 3 (LamVar 3)) (LamAbs 4 (LamVar 4)))
lamExpr3 = LamApp lamExpr2 lamExpr1
lamExpr4 = LamApp lamExpr1 lamExpr2
lamExpr5 = (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3))
lamExpr6 = LamApp lamExpr5 (LamApp (LamAbs 4 (LamVar 4)) (LamVar 5)) 