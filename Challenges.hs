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
--import Text.Parsec.Prim
--import Text.Parsec.Combinator

--Data type definition for 'let' statements
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show, Eq)

--Data type definition for lambda expressions
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show, Eq)


-- Challenge 1
--Convert a list of integers and an expression (that is, the first and second arguments of a 'let' statement)
--into a lambda expression, to use in the main parsing function 

--Function to convert the list of integers given as the first parameter of a 'let' expression
convertList :: [Int] -> Expr -> LamExpr
convertList [x] e1 = LamAbs x(convertLet e1)
convertList xs e1 = LamAbs (head xs) (convertList (tail xs) e1)

convertLet :: Expr -> LamExpr
convertLet (Var x) = (LamVar x)
convertLet (App e1 e2) = LamApp (convertLet e1) (convertLet e2)
convertLet (Let [x] (Var e1) (Var e2)) = LamApp (LamAbs x(LamVar e2))(LamVar e1) 
convertLet (Let [x] (e1) (e2)) = LamApp (LamAbs x(convertLet e2))(convertLet e1) 
convertLet (Let [x,y] e1 e2) = LamApp (LamAbs x(convertLet e2)) (LamAbs y(convertLet e1))
convertLet (Let xs e1 e2) = LamApp (LamAbs (head xs)(convertLet e2)) (convertList (tail xs) e1)

-- Challenge 2
-- pretty print a let expression by converting it to a string

printList :: [Int] -> String
printList [x] = "x"++(show x)
printList xs = printList [(head xs)] ++ " " ++ printList(tail xs)


prettyPrint :: Expr -> String
prettyPrint (Var x) = "x"++ show x
prettyPrint (Let xs y z) = "let " ++ printList xs ++ " = " ++ prettyPrint y ++ " in " ++ prettyPrint z
prettyPrint (App e@(Let xs e1 e2) f@(Let ys e3 e4)) = prettyPrint e ++ " " ++ prettyPrint f
prettyPrint (App e@(App e1 e2) f@(App e3 e4)) = (prettyPrint e) ++ " " ++ "(" ++ (prettyPrint f) ++ ")"
prettyPrint (App e1 e@(App e2 e3)) = (prettyPrint e1) ++ " " ++ "(" ++ (prettyPrint e) ++ ")"
prettyPrint (App (App e1 e2) e3) = prettyPrint (App e1 e2) ++ " " ++ prettyPrint e3
prettyPrint (App e@(Var x) e2) = (prettyPrint e) ++ " " ++ (prettyPrint e2)
prettyPrint (App x y) | length (prettyPrint x) == 2 && length (prettyPrint y) > 2 = prettyPrint x ++ " " ++ "(" ++ prettyPrint y ++ ")"
                      | length (prettyPrint y) == 2 && length (prettyPrint x) > 2 = "(" ++ prettyPrint x ++ ")" ++ " " ++ prettyPrint y 
                      | length (prettyPrint x) == 2 && length (prettyPrint y) == 2 = prettyPrint x ++ " " ++ prettyPrint y
                      | otherwise = "(" ++ prettyPrint x ++ ")" ++ "(" ++ prettyPrint y ++ ")" 



-- Challenge 3
-- parse a let expression

         
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False


nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)


intExpr :: Parser Int
intExpr = do symbol("x")
             n <- natural
             return (n)


intExpr' :: Parser [Int]
intExpr' = do symbol("x")
              n <- natural
              e <- many (do symbol "x"
                            natural)
              return (n:e)


varExpr :: Parser Expr
varExpr = do symbol("x")
             n <- natural
             return (Var n)


letExpr :: Parser Expr
letExpr = do symbol("let")
             e1 <- intExpr'
             symbol("=")
             e2 <- expr
             symbol("in")
             e3 <- expr
             return (Let e1 e2 e3)

letExpr' :: Parser Expr
letExpr' = do symbol("let")
              e1 <- intExpr'
              symbol("=")
              e2 <- expr
              symbol("in")
              symbol ("(")
              e3 <- expr
              symbol (")")
              return (Let e1 e2 e3)

letExpr'' :: Parser Expr
letExpr'' = do symbol("let")
               e1 <- intExpr'
               symbol("=")
               symbol ("(")
               e2 <- expr
               symbol (")")
               symbol("in")
               e3 <- expr
               return (Let e1 e2 e3)

letExpr''' :: Parser Expr
letExpr''' = do symbol("let")
                e1 <- intExpr'
                symbol("=")
                symbol ("(")
                e2 <- expr
                symbol (")")
                symbol("in")
                symbol ("(")
                e3 <- expr
                symbol (")")
                return (Let e1 e2 e3)

appExpr :: Parser Expr
appExpr = do e1 <- lowerExpr
             e2 <- expr
             return (App e1 e2)

appExpr1 :: Parser Expr
appExpr1 = do symbol ("(")
              e1 <- lowerExpr
              symbol (")")
              symbol ("(")
              e2 <- expr
              symbol (")")
              return (App (e1) (e2))

appExpr2 :: Parser Expr
appExpr2 = do symbol ("(")
              e1 <- lowerExpr
              symbol (")")
              e2 <- expr
              return (App (e1) (e2))

appExpr3 :: Parser Expr
appExpr3 = do e1 <- lowerExpr
              symbol ("(")
              e2 <- expr
              symbol (")")
              return (App (e1) (e2))


appExpr4 :: Parser Expr
appExpr4 = do e1 <- appExpr3
              e2 <- expr
              return (App (e1) (e2))

appExpr5 :: Parser Expr
appExpr5 = do e1 <- lowerExpr
              e2 <- lowerExpr
              e3 <- expr
              return (App (App e1 e2) e3)


appExpr6 :: Parser Expr
appExpr6 = lowerExpr `chainl1'` appOperatorParser

appOperatorParser :: Parser (Expr -> Expr -> Expr)
appOperatorParser = do e1 <- lowerExpr
                       e2 <- lowerExpr
                       return appOperatorFunction

appOperatorFunction :: Expr -> Expr -> Expr
appOperatorFunction e1 e2 = (App e1 e2)


testExpr = many (symbol("x") >> natural)


letParser :: Parser Expr
letParser = letExpr <|> letExpr' <|> letExpr'' <|> letExpr'''

appParser :: Parser Expr
appParser =  appExpr4 <|> appExpr3 <|> appExpr1 <|> appExpr2 <|> appExpr 

expr :: Parser Expr
expr = appExpr6 <|> lowerExpr 
lowerExpr = appParser <|> evenLowerExpr 
evenLowerExpr = varExpr <|> letParser 



-------- chain1l--------------

chainl1' p op = foldl (\x f -> f x) <$> p <*> many ((\f y -> flip f y) <$> op <*> p)

-----------------------------


parseLet :: String -> Maybe Expr
parseLet s | length (parse expr s) == 0 = Nothing
           | otherwise = Just (fst(head(parse expr s)))


-- parseLet "x1 (x2 x3) x4" --> App (App (Var 1) (App (Var 2) (Var 3))) (Var 4)

-- Challenge 4
-- count reductions using two different strategies 

--(LamApp  (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3)) (LamApp (LamAbs 4 (LamVar 4)) (LamVar 5)))


rename x = x+1

free :: Int -> LamExpr -> Bool
free x (LamVar y) = x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2) = (free x e1) || (free x e2)

subst :: LamExpr -> Int -> LamExpr -> LamExpr 
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e |
    x /= y && not (free x e) = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e |
    x /= y && (free x e) = let x' = rename x in 
        subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e | x == y = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e) 

reductions :: (LamExpr -> LamExpr) -> LamExpr -> [ (LamExpr, LamExpr) ]
reductions ss e = [ p | p <- zip evals (tail evals) ]
   where evals = iterate ss e

eval :: (LamExpr -> LamExpr) -> LamExpr -> LamExpr
eval ss = fst . head . dropWhile (uncurry (/=)) . reductions ss

trace :: (LamExpr -> LamExpr) -> LamExpr -> [LamExpr]
trace ss  = (map fst) . takeWhile (uncurry (/=)) .  reductions ss


evalright = eval evalR
tracer = trace evalR
evalleft = eval evalL
tracel = trace evalL

lambdaExpr5 = (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3))
lambdaExpr6 = LamApp lambdaExpr5 (LamApp (LamAbs 4 (LamVar 4)) (LamVar 5)) 


evalL :: LamExpr -> LamExpr
evalL (LamAbs x e) = (LamAbs x e)
evalL (LamApp (LamAbs x e1) e@(LamAbs y e2)) = subst e1 x e
evalL (LamApp (LamAbs x (LamAbs y (LamVar f))) e@(LamVar z)) = LamAbs y e
evalL (LamApp e@(LamVar x) (LamApp e1 e2)) = e
evalL (LamApp e@(LamAbs x e1) f@(LamVar y)) = f
evalL (LamApp e@(LamAbs x e1) f@(LamApp (LamAbs y e2)g@(LamVar w))) = subst e1 x g
evalL (LamApp e@(LamAbs x e1) e2) = LamApp e (evalL e2)
evalL (LamApp e1 e2) = LamApp (evalL e1) e2
evalL (LamVar x) = (LamVar x) 


evalR :: LamExpr -> LamExpr
evalR (LamAbs x e) = (LamAbs x e)
evalR (LamApp (LamAbs x e1) e@(LamAbs y e2)) = subst e1 x e
evalR (LamApp e@(LamVar x) (LamApp e1 e2)) = e
evalR (LamApp e@(LamAbs x e1) f@(LamVar y)) = subst e1 x f
evalR (LamApp e@(LamAbs x e1) e2) = LamApp e (evalR e2)
evalR (LamApp e1 e@(LamVar x)) = (LamApp (evalR e1) e)
evalR (LamApp e1 e2) = LamApp e1 (evalR e2)
evalR (LamVar x) = (LamVar x) 


leftmost e = length (tracel e)
rightmost e = length (tracer e)


countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
countReds e limit | ((leftmost e) > limit) && ((rightmost e) > limit) = (Nothing, Nothing)
                  | ((leftmost e) > limit) = (Nothing, Just (rightmost e))
                  | ((rightmost e) > limit) = (Just (leftmost e), Nothing)
                  | otherwise = (Just (leftmost e), Just (rightmost e))


-- Challenge 5
-- compile an arithmetic expression into a lambda calculus equivalent

compileInt' :: Int -> LamExpr
compileInt' s | s == 0 = (LamAbs 2 (LamVar 2)) 
             | s == 1 = (LamApp (LamVar 1) (LamVar 2))
             | s == 2 = (LamApp (LamVar 1) (LamApp (LamVar 1) (LamVar 2))) 
             | s < 0 = (LamAbs 0 (LamVar 0)) 
             | otherwise =  (LamApp (LamVar 1) (compileInt' ((s)-1))) 
             

compileInt :: String -> Maybe LamExpr
compileInt s | (read s) == 0 = Just(LamAbs 1 (LamAbs 2 (LamVar 2))) 
             | otherwise = Just(LamAbs 1 (LamAbs 2 (compileInt' (read s))))
               


compileArith :: String -> Maybe LamExpr
compileArith s = Nothing
    

