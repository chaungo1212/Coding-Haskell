
-- Assignment 4 Part I, CSCE-314
-- Section: 502

-- (Acknowledge any help received here)

module Main where

import Prelude hiding (lookup)

import Test.HUnit
import System.Exit

-- Haskell data types for W
data WValue = VInt Int 
            | VBool Bool
            | VMarker
              deriving (Eq, Show)

data WExp = Val WValue
          | Var String

          | Plus WExp WExp
          | Mult WExp WExp

          | Equals      WExp WExp
          | NotEqual    WExp WExp
          | Less        WExp WExp
          | Greater     WExp WExp
          | LessOrEq    WExp WExp
          | GreaterOrEq WExp WExp

          | And  WExp WExp
          | Or   WExp WExp
          | Not  WExp
            deriving Show

data WStmt = Empty
           | VarDecl String WExp
           | Assign  String WExp
           | If      WExp   WStmt WStmt
           | While   WExp   WStmt
           | Block  [WStmt]
             deriving Show

type Memory = [(String, WValue)]
marker = ("|", VMarker)
isMarker (x, _) = x == "|"

-- eval function
eval :: WExp -> Memory -> WValue
{-eval (Val (VInt i)) t = (VInt i)
eval (Val (VBool b)) t = (VBool b)

eval (Var a) t = fromJust (lookup a t)-}

eval ( Val x ) t = x
eval ( Var s ) xs = case lookup z xs of
                      Nothing -> error (z ++ "not Found")
                      Just v -> v

eval (Plus a b) t = plus (eval a t) (eval b t)
eval (Mult a b) t = mult (eval a t) (eval b t)
eval (Equals a b) t = equals (eval a t)(eval b t)
eval (NotEqual a b) t = notequal (eval a t)(eval b t)
eval (Less a b) t = less (eval a t)(eval b t)
eval (LessOrEq a b) t = lessoreq(eval a t)(eval b t)
eval (Greater a b) t = greater (eval a t)(eval b t)
eval (GreaterOrEq a b) t = greateroreq (eval a t)(eval b t)
eval (And a b) t = and (eval a t)(eval b t)
eval (Or a b) t = or (eval a t)(eval b t)
eval (Not a) t = not (eval a t)


plus (VInt c)(VInt d) = VInt (c + d)
plus _ _ = error "Addition Error"

mult(VInt c)(VInt d) = VInt (c * d)
mult _ _ = error "Multiplication Error"

equals (VInt c)(VInt d) = VBool (c == d)
-- equals (VBool e)(VBool f) = VBool $ e == f
equals _ _ = error "Equals Error"

notequal (VInt c)(VInt d) = VBool (c /= d)
--notequal (VBool e)(VBool f) = VBool $ e /= f
notequal _ _ = error "Not Equal Error"

less (VInt c)(VInt d) = VBool (c < d)
--less (VBool e)(VBool f) = VBool $ e < f
less _ _ = error "Less Error"

lessoreq (VInt c)(VInt d) = VBool (c <= d)
--lessoreq (VBool e)(VBool f) = VBool $ e <= f
lessoreq _ _ = error "LessOrEqual Error"

greater (VInt c)(VInt d) = VBool (c > d)
--greater (VBool e)(VBool f) = VBool $ e > f
greater _ _ = error "Greater Error"


greateroreq (VInt c)(VInt d) = VBool (c >= d)
--greateroreq (VBool e)(VBool f) = VBool $ e >= f
greateroreq _ _ = error "GreaterOrEqual Error"

and (VBool c)(VBool d) = VBool (x && y)
and _ _ = error "And Error "

or (VBool c)(VBool d) = VBool (a || b)
or _ _ = error "Or Error"

not (VBool a)= VBool (not a)
not _ = error "Not Error"




-- exec function
exec :: WStmt -> Memory -> Memory
exec  Empty em = em
exec (VarDecl str w) var
          | (lookup str var) == Nothing = (str, (eval w var)) : var
          | otherwise = error ("'" ++ str ++ "' already declared")

exec (Assign str w) var
          | (lookup str var) == Nothing = error ("'" ++ str ++ "' not declared")
          | otherwise = (str, (eval w var)) : var

exec (If w1 s1 s2) var
          | eval w1 var == VBool(True)  = exec s1 var
          | eval w1 var == VBool(False) = exec s2 var

exec (While w1 s1) var = if(eval w1 var == VBool(True)) then exec (While w1 s1) (exec s1 var) else var

exec (Block []) var = var
exec (Block (x:xs)) var = exec (Block xs) (exec x var)




-- example programs
prog1 = Block
   [
     VarDecl "x" (Val (VInt 0)),
     VarDecl "y" (Val (VInt 1)),
     VarDecl "b" (Greater (Var "x") (Val (VInt 0))),
     If (Or (Var "b") (Not (GreaterOrEq (Var "x") (Val (VInt 0)))))
        ( Block [ Assign "x" (Val (VInt 1)),
                  Assign "y" (Plus (Var "y") (Val (VInt 1)))
                ] 
        )
        ( Assign "x" (Val (VInt 2)) )
  ]

factorial = Block
  [
     VarDecl "acc" (Val (VInt 1)),
     While (Greater (Var "arg") (Val (VInt 0)))
     ( Block
       [ Assign "acc" (Mult (Var "acc") (Var "arg")),
         Assign "arg" (Plus (Var "arg") (Val (VInt (-1))))         
       ]
     ),
     Assign "result" (Var "acc")
  ]

-- some useful helper functions
lookup s [] = Nothing
lookup s ((k,v):xs) | s == k = Just v
                    | otherwise = lookup s xs

asInt (VInt v) = v
asInt x = error $ "Expected a number, got " ++ show x

asBool (VBool v) = v
asBool x = error $ "Expected a boolean, got " ++ show x

fromJust (Just v) = v
fromJust Nothing = error "Expected a value in Maybe, but got Nothing"

-- unit tests
myTestList =
  TestList [
    test $ assertEqual "prog1 test" [] (exec prog1 []),

    let res = lookup "result" (
                exec factorial [("result", VInt (-1)), ("arg", VInt 10)])
    in test $ assertBool "factorial of 10" (3628800 == asInt (fromJust res))
    ]    

-- main: run the unit tests  
main = do c <- runTestTT myTestList
          putStrLn $ show c
          let errs = errors c
              fails = failures c
          if (errs + fails /= 0) then exitFailure else return ()

