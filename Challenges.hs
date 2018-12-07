-- COMP2209 Coursework 2, University of Southampton 2018
-- DUMMY FILE FOR YOU TO EDIT AND ADD YOUR OWN IMPLEMENTATIONS
-- NOTE THAT NO THIRD PARTY MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION TYPE SIGNATURES NOR TYPE DEFINITIONS 
-- This module statement makes public only the specified functions and types
-- DO NOT CHANGE THIS LIST OF EXPORTED FUNCTIONS AND TYPES
module Challenges (convertLet, prettyPrint, parseLet, countReds, compileArith, reductionscbn, eval1cbn, tracecbn, makeAbsChain, parseExprToList,
    Expr(App, Let, Var), LamExpr(LamApp, LamAbs, LamVar)) where

import Data.Char
import Parsing

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

-- convert a let expression to lambda expression
convertLet :: Expr -> LamExpr
-- replace the definition below with your solution
--convertLet e = (LamVar 0)

--Case where both e1 and e2 are let expressions
convertLet (Let x (Let x1 e1 e2) (Let x1' e1' e2'))
    | length x == 1 = LamApp (LamAbs (x!!0) (convertLet (Let x1' e1' e2'))) (convertLet (Let x1' e1' e2'))

--Case where e1 is a let expression
convertLet (Let x (Let x' e1' e2') e2)
    | length x == 1 = LamApp (LamAbs (x!!0) (listToApp e2vars)) (convertLet (Let x' e1' e2'))
    | length x > 1 = LamApp (LamAbs (x!!0) (listToApp e2vars)) (makeAbsChain (tail x) (parseExprToList e1'))
    where
        e2vars = parseExprToList e2

--Case where e2 is a let expression
convertLet (Let x e1 (Let x' e1' e2'))
    | length x == 1 = LamApp (LamAbs (x!!0) (convertLet (Let x' e1' e2'))) (listToApp e1vars)
    | length x > 1 = LamApp (LamAbs (x!!0) (convertLet (Let x' e1' e2'))) (makeAbsChain (tail x) (e1vars))
    where
        e1vars = parseExprToList e1

--Case where neither e1 or e2 are let expressions
convertLet (Let x e1 e2)
    | length x == 1 = LamApp (LamAbs (x!!0) (listToApp e2vars)) (listToApp e1vars)
    | length x > 1 = LamApp (LamAbs (x!!0) (listToApp e2vars)) (makeAbsChain (tail x) e1vars)
    where 
        e1vars = parseExprToList e1
        e2vars = parseExprToList e2
    
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

-- Challenge 2
-- pretty print a let expression by converting it to a string
prettyPrint :: Expr -> String
-- replace the definition below with your solution
prettyPrint e = ""

-- Challenge 3
-- parse a let expression
parseLet :: String -> Maybe Expr
-- replace the definition below with your solution
parseLet s = Nothing

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