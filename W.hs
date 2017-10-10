
-- Assignment 4 Part II, CSCE-314
-- Full implementation of the interpreter not provided yet.
-- The contents below is the same as the skeleton for Part I with
-- few modification for Part II.

module W ( WValue(..), WExp(..), WStmt(..), exec, eval) where

-- Haskell data types for W
data WValue = VInt Int 
            | VBool Bool
            | VString String -- new for Part II
            | VMarker
              deriving Eq

instance Show WValue where
    show (VInt i)    = show i
    show (VBool b)   = show b
    show (VString s) = s
    show (VMarker)   = "_"

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
           | Print WExp   -- new for Part II
             deriving Show

type Memory = [(String, WValue)]
marker = ("|", VMarker)
isMarker (x, _) = x == "|"

plus :: WValue -> WValue -> WValue
plus (VInt c)(VInt d) = VInt (c + d)
plus _ _ = error "Addition Error"


mult :: WValue -> WValue -> WValue
mult(VInt c)(VInt d) = VInt (c * d)
mult _ _ = error "Multiplication Error"


equals :: WValue -> WValue -> WValue
equals (VInt c)(VInt d) = VBool (c == d)
equals _ _ = error "Equals Error"

notequal :: WValue -> WValue -> WValue
notequal (VInt c)(VInt d) = VBool (c /= d)
notequal _ _ = error "Not Equal Error"

less :: WValue -> WValue -> WValue
less (VInt c)(VInt d) = VBool (c < d)
less _ _ = error "Less Error"

lessoreq :: WValue -> WValue -> WValue
lessoreq (VInt c)(VInt d) = VBool (c <= d)
lessoreq _ _ = error "LessOrEqual Error"


greater :: WValue -> WValue -> WValue
greater (VInt c)(VInt d) = VBool (c > d)
greater _ _ = error "Greater Error"


greateroreq :: WValue -> WValue -> WValue
greateroreq (VInt c)(VInt d) = VBool (c >= d)
greateroreq _ _ = error "GreaterOrEqual Error"


nand :: WValue -> WValue -> WValue
nand (VBool a) (VBool b) = VBool ( a && b )
nand _ _ = error "AND Error"

nor :: WValue -> WValue -> WValue
nor (VBool a) (VBool b) = VBool ( a || b)
nor _ _ = error "OR Error"

nnot :: WValue -> WValue
nnot (VBool a) = VBool ( not a )
nnot _ = error "NOT Error"

-- eval function
eval :: WExp -> Memory -> WValue
eval ( Val x ) a = x
eval ( Var t ) xs = case lookup t xs of
                      Nothing -> error (t ++ "not Found")
                      Just v -> v
eval (Plus w1 w2) a = plus (eval w1 a) (eval w2 a)
eval (Mult w1 w2) a = mult (eval w1 a) (eval w2 a)
eval (Equals w1 w2) a = equals (eval w1 a) (eval w2 a)
eval (NotEqual w1 w2) a = notequal (eval w1 a) (eval w2 a)
eval (Less w1 w2) a = less (eval w1 a) (eval w2 a)
eval (Greater w1 w2) a = greater (eval w1 a) (eval w2 a)
eval (LessOrEq w1 w2) a = lessoreq (eval w1 a) (eval w2 a)
eval (GreaterOrEq w1 w2) a = greateroreq(eval w1 a) (eval w2 a)
eval (And w1 w2) a = nand (eval w1 a) (eval w2 a)
eval (Or w1 w2) a = nor (eval w1 a) (eval w2 a)
eval (Not w1) a = nnot (eval w1 a)



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
asInt (VInt v) = v
asInt x = error $ "Expected a number, got " ++ show x

asBool (VBool v) = v
asBool x = error $ "Expected a boolean, got " ++ show x

fromJust (Just v) = v
fromJust Nothing = error "Expected a value in Maybe, but got Nothing"

{--
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
--}
