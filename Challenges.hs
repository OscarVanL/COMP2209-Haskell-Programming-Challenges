-- COMP2209 Coursework 2, University of Southampton 2018
-- DUMMY FILE FOR YOU TO EDIT AND ADD YOUR OWN IMPLEMENTATIONS
-- NOTE THAT NO THIRD PARTY MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION TYPE SIGNATURES NOR TYPE DEFINITIONS 
-- This module statement makes public only the specified functions and types
-- DO NOT CHANGE THIS LIST OF EXPORTED FUNCTIONS AND TYPES
module Challenges (convertLet, prettyPrint, parseLet, countReds, compileArith,
    Expr(App, Let, Var), LamExpr(LamApp, LamAbs, LamVar)) where

import Data.Char
import Parsing

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

-- convert a let expression to lambda expression
convertLet :: Expr -> LamExpr
convertLet e
    | (Let x (Let x1 e1 e2) (Let x1' e1' e2')) <- e = LamApp (LamAbs (x!!0) (convertLet (Let x1' e1' e2'))) (prefixAbsChain (tail x) (convertLet (Let x1' e1' e2')))
    | (Let x (Let x1' e1' e2') e2) <- e             = LamApp (LamAbs (x!!0) (listToApp (parseExprToList e2))) (prefixAbsChain (tail x) (convertLet (Let x1' e1' e2')))
    | (Let x e1 (Let x' e1' e2')) <- e              = LamApp (LamAbs (x!!0) (convertLet (Let x' e1' e2'))) (makeAbsChain (tail x) (parseExprToList e1))
    | (Let x e1 e2) <- e                            = LamApp (LamAbs (x!!0) (listToApp (parseExprToList e2))) (makeAbsChain (tail x) (parseExprToList e1))
    
parseExprToList :: Expr -> [Int]
parseExprToList (App a b) = (parseExprToList a) ++ (parseExprToList b)
parseExprToList (Var x) = [x]

listToApp :: [Int] -> LamExpr
listToApp (x:xs)
    | length (x:xs) == 1 = LamVar x
    | length (x:xs) > 1 = LamApp (LamVar x) (listToApp xs)

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
    --Handles all permetations of lets
    | (Let x (Let x1 e1 e2) (Let x1' e1' e2')) <- e = "let " ++ parseListToString x ++ " = " ++ prettyPrint (Let x1 e1 e2) ++ " in " ++ prettyPrint (Let x1' e1' e2')
    | (Let x (Let x1' e1' e2') e2) <- e             = "let " ++ parseListToString x ++ " = " ++ prettyPrint (Let x1' e1' e2') ++ " in " ++ prettyPrint e2
    | (Let x e1 (Let x' e1' e2')) <- e              = "let " ++ parseListToString x ++ " = " ++ prettyPrint e1 ++ " in " ++ prettyPrint (Let x' e1' e2')
    | (Let x e1 e2) <- e                            = "let " ++ parseListToString x ++ " = " ++ prettyPrint e1 ++ " in " ++ prettyPrint e2
    --Handles all permetations of Apps and Vars
    | (App x y) <- e                                = parseExprToString (App x y)
    | (Var x) <- e                                  = "x" ++ (show x)

parseListToString :: [Int] -> String
parseListToString (x:xs)
    | length (x:xs) == 0 = ""
    | length (x:xs) == 1 = "x" ++ (show x)
    | length (x:xs) > 1  = "x" ++ (show x) ++ " " ++ parseListToString (xs)

parseExprToString :: Expr -> String
parseExprToString e
    | (App (App a a') (App b b')) <- e          = "(" ++ parseExprToString (App a a') ++ ") (" ++ parseExprToString (App b b') ++ ")"
    | (App (Var a) (App b b')) <- e             = parseExprToString (Var a) ++ " (" ++ parseExprToString (App b b') ++ ")"
    | (App (App a a') (Var b)) <- e             = parseExprToString (App a a') ++ " " ++ parseExprToString (Var b)
    | (App (Var x) (Var y)) <- e                = parseExprToString (Var x) ++ " " ++ parseExprToString (Var y)
    | (App (Let x e1 e2) (Var b)) <- e          = "(" ++ prettyPrint (Let x e1 e2) ++ ") " ++ parseExprToString (Var b)
    | (App (Var a) (Let x e1 e2)) <- e          = parseExprToString (Var a) ++ " (" ++ prettyPrint (Let x e1 e2) ++ ")"
    | (App (Let x e1 e2) (Let x' e1' e2')) <- e = "(" ++ prettyPrint (Let x e1 e2) ++ ") (" ++ prettyPrint (Let x' e1' e2') ++ ")"
    | (Var x) <- e                         = "x" ++ (show x)

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
    expr <- parseStrToApp <|> parseStrToLet <|> parseStrToVar
    _ <- char ')'
    _ <- space
    return expr

--Parser checks a var expression following correct syntax.
parseStrToVar :: Parser Expr
parseStrToVar = do
    expr <- parseStrToInt
    return (Var expr)

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
countReds e limit = (Nothing, Nothing)

-- Challenge 5
-- compile an arithmetic expression into a lambda calculus equivalent
compileArith :: String -> Maybe LamExpr
-- replace the definition below with your solution
compileArith s = Nothing
    

----------------------------------------------

--Borrowed from Lecture 13 slides
eval1cbn :: LamExpr -> LamExpr
--No reduction
eval1cbn (LamAbs x e) = (LamAbs x e)
--Beta reduction
eval1cbn (LamApp (LamAbs x e1) e2) = subst e1 x e2
eval1cbn (LamApp e1 e2) = LamApp (eval1cbn e1) e2

--Borrowed from Lecture 13 slides
tracecbn :: LamExpr -> [ LamExpr ]
tracecbn = (map fst) . takeWhile (uncurry (/=)) . reductionscbn

--Borrowed from Lecture 13 slides
reductionscbn :: LamExpr -> [ (LamExpr, LamExpr) ]
reductionscbn e = [ p | p <- zip evals (tail evals) ]
    where evals = iterate eval1cbn e

--Borrowed from Lecture 13 slides
subst :: LamExpr -> Int -> LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e |
 x /= y && not (free x e) = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e |
    x /=y && (free x e) = let x' = rename x in
            subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e | x == y = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e) 

rename :: Int -> Int
rename x = read ((show x) ++ "0")

--Borrowed from Lecture 13 slides
free :: Int -> LamExpr -> Bool
free x (LamVar y) = x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)