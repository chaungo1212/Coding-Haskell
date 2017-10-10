
-- This is head comment (comment should be preceded by two dashes)
-- Assignment 2, CSCE 314 Section 502
-- Student Name: Chau Ngo
-- UIN: 524006756
-- (Acknowledge any help received here): peer teacher central

module Main where

import Test.HUnit
import System.Exit

-- Problem 2
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci (n-2)

-- Problem 3
myProduct :: [Integer] -> Integer
myProduct[]=1
myProduct (n:ns) = n* product ns

-- Problem 4
flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

-- Problem 5
myLength :: [a] -> Int
myLength [] = 0
myLength(_:xs) = 1 + myLength xs

-- Problem 6
quicksort :: Ord t => [t] -> [t]
quicksort [] = []
quicksort(x:xs) = quicksort smaller ++ [x] ++ quicksort larger
 where
     smaller = [a|a <- xs, a <= x]
     larger = [b|b <- xs, b > x] 

-- Problem 7
type Set a = [a]
isElement :: Eq a => a -> [a] -> Bool
isElement x [] = False                                                                                                                                   
isElement x (y:ys) = if(x==y) then True else isElement x ys 

--Problem 8 
removeEven :: [Integer] -> [Integer]
removeEven [] =,[]
removeEven (x:xs) = [t|t <- xs, t `mod` 2 == 1]

--Problem 10
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (take (length (factors x) - 1) (factors x)) == x]


--Problem 11
replicate' :: Int -> a -> [a]
replicate' n a = [a | _ <- [1..n]]

myTestList = 
  TestList [ 
    "fibonacci" ~: fibonacci 4 ~=? 3

    , "myProduct" ~: myProduct [1..10] ~=? 3628800
    
    , "flatten 1" ~: flatten [[]::[Int]] ~=? []
    , "flatten 2" ~: flatten [[]::[Int], [], []] ~=? []
    , "flatten 3" ~: flatten [[1], [2, 3, 4], [], [5, 6]] ~=? [1, 2, 3, 4, 5, 6]
      
    , "myLength" ~: myLength [1, 2, 3] ~=? 3

    , "quicksort 1" ~: quicksort [3, 2, 5, 1, 6] ~=? [1,2,3,5,6]
    , "quicksort 2" ~: quicksort "howdy" ~=? "dhowy"
    
    , "isElement 1" ~: (isElement 'c' "abcd") ~=? True
    , "isElement 2" ~: (isElement 'e' "abcd") ~=? False
   
    , "replicate' " ~: (replicate' 3 True) ~=? [True, True, True]
  
    , "perfects" ~: (perfects 500)~=? [6, 28, 496]
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
