
-- Assignment 3, CSCE-314
-- Section: PUT YOUR SECTION (502) HERE
-- Student Name: Chau Ngo	
-- UIN: 524006756
-- (Acknowledge any help received here):book,lecture notes and peer teacher central

module Main where

import Test.HUnit
import System.Exit
import Data.List
import Data.Char 


---- Part 1.
-- Problem 1
cutSpace[]  = []
cutSpace (' ':a)= cutSpace(a)
cutSpace (a:b)=a:b
cutWhitespace xs = map cutSpace xs
-- Problem 2
multList :: [Int]->[Int] ->[Int]
multList xs ys =  zipWith (*) xs ys
multListt :: [[Int]]->[[Int]] ->[[Int]]
multListt xss yss =  zipWith (multList) xss yss

---- Part 2.
-- Problem 3
mergeBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeBy predicate xs [] = xs
mergeBy predicate [] ys = ys
mergeBy predicate (x:xs) (y:ys) 
                          | predicate x y = x:mergeBy predicate xs (y:ys)
                          | otherwise = y : mergeBy predicate (x:xs) ys
halves :: [a] -> ([a],[a])
halves xs = splitAt (length xs `div` 2) xs

msortBy :: (a -> a -> Bool) -> [a] -> [a]
msortBy predicate[] = []
msortBy predicate[x] =[x]
msortBy predicate ls = mergeBy predicate (msortBy predicate xs) (msortBy predicate ys)
          where (xs, ys) = halves ls
mergeSort :: Ord a => [a] -> [a]
mergeSort = msortBy (<)
-- Problem 4
multiply :: [Int] -> Int
multiply xs = foldr (*) 1 xs

-- Problem 5
concatenate :: [String] -> String     
concatenate [] = ""
concatenate xs = foldl (++) "" xs 
-- Problem 6
concatenateAndUpcaseOddLengthStrings :: [String] -> String
concatenateAndUpcaseOddLengthStrings =concatenate.(map(map toUpper)). filter(\q -> odd (length q))
-- Problem 7
myInsert :: Ord a => a -> [a] -> [a]
myInsert x[] = [x]
myInsert x (y:ys) 
            | x <= y = x : y : ys
            |otherwise = y : myInsert x ys
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr (myInsert) []

-- Problem 8
maxElem :: Ord a => [a] -> a 
maxElem = foldr1 max  

---- Part 3.
data Tree a b = Branch b (Tree a b) (Tree a b)
              | Leaf a

-- Problem 9
instance (Show a, Show b) => Show (Tree a b) where
  show t = showInd "" t
    where showInd pre (Leaf a) = pre ++ show a ++ "\n"
          showInd pre (Branch a l r) = 
            pre ++ (show a) ++ "\n" ++ 
            showInd (pre ++ "  ") l ++
            showInd (pre ++ "  ") r 
-- Problem 10
preorder  :: (a -> c) -> (b -> c) -> Tree a b -> [c]
preorder f g (Leaf a) = [f a]
preorder f g (Branch a l r )= g a : (preorder f g l  ++ preorder f g r)

inorder   :: (a -> c) -> (b -> c) -> Tree a b -> [c]
inorder f g (Leaf a) = [f a]
inorder f g (Branch a l r) = inorder f g l ++ [g a] ++ inorder f g r

---- Part 4. 
data E = IntLit Int
       | BoolLit Bool
       | Plus E E    
       | Mult E E    
       | Equals E E
         deriving (Eq, Show)

-- Problem 11
eval :: E -> E
eval (IntLit a) = IntLit a
eval (BoolLit b) = BoolLit b
eval (Plus a b) = plus (eval a) (eval b)
eval (Mult a b) = mult (eval a) (eval b)
eval (Equals a b) = equals (eval a) (eval b)
plus (IntLit a) (IntLit b) = IntLit $ a + b
plus _ _ = error "Error "

mult (IntLit a) (IntLit b) = IntLit $ a * b
mult _ _ = error "Error"

equals (IntLit a) (IntLit b) = BoolLit $ a == b
equals (BoolLit a) (BoolLit b) = BoolLit $ a == b
equals _ _ = error "Error"
mytree = Branch "A" 
           (Branch "B" 
              (Leaf 1) 
              (Leaf 2)) 
           (Leaf 3)

prog1 = Equals 
           (Plus (IntLit 1) (IntLit 9))
           (Mult
              (IntLit 5)
              (Plus (IntLit 1) (IntLit 1)))

prog2 = Equals
           (Equals
              (Mult (IntLit 4) (IntLit 2))
              (Plus (IntLit 5) (Mult (IntLit 2) (IntLit 1))))
           (Equals (BoolLit True) (BoolLit True))

   
 
myTestList =
  let te s e a = test $ assertEqual s e a
      tb s b = test $ assertBool s b
  in
    TestList [ 
        te "cutWhitespace" ["x","y","z"] (cutWhitespace [" x","y"," z"])
      , te "multListt" [[3,2,2],[9,16,30],[5,8,9]] (multListt [[1,1,1],[3,4,6],[1,2,3]] [[3,2,2],[3,4,5],[5,4,3]])
      , te "mergeBy 1" "GFEDBA" (mergeBy (>) "FED" "GBA")
      , te "mergeBy 2" "HMaouiwdy" (mergeBy (<) "Howdy" "Maui")
      
      , te "msortBy 1" " 'eggim" (msortBy (<) "gig 'em") 
      , te "msortBy 2" "nmlkieecbbaJ  " (msortBy (>) "Jack be nimble")
      , te "msortBy 3" "" (msortBy (<) "")
      
      , te "mergeSort 1" " adhllowy" (mergeSort "howdy all") 
      , te "mergeSort 2" "" (mergeSort "") 
      , te "mergeSort 3" "x" (mergeSort "x") 

      , te "multiply" 10 (multiply [-2, -1, 5])
      
      , te "concatenate" "ABCD" (concatenate ["AB", "", "", "C", "D", ""])

      , te "concatenateAndUpcaseOddLengthStrings"
          "HERE'S AN EXAMPLE" (concatenateAndUpcaseOddLengthStrings ["here's ", "an ", "a ", "example"])

      , te "myInsert 1" "How are you?" (myInsert 'o' "Hw are you?")
      , te "myInsert 2" "abcdefg" (myInsert 'c' "abdefg")
      , te "insertionSort" "  Jabcceikkqu" (insertionSort "Jack be quick")
      
      , te "max in a list" 200 (maxElem [3, 10, 200, 42])

      , te "preorder" "AB123" (concatenate (preorder show id mytree))
      , te "inorder" "1B2A3" (concatenate (inorder show id mytree))

      , te "eval1" (BoolLit True) (eval prog1)
      , te "eval2" (BoolLit False) (eval prog2)
      ]

main = do c <- runTestTT myTestList
          putStrLn $ show c
          let errs = errors c
              fails = failures c
          exitWith (codeGet errs fails)
          
codeGet errs fails
 | fails > 0       = ExitFailure 2
 | errs > 0        = ExitFailure 1
 | otherwise       = ExitSuccess
