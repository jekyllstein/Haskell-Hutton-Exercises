{- 6. Modify the final program to:
        a. allow the use of exponentiation in expressions;
        b. produce the nearest solutions if no exact solution is possible;
        c. order the solutions using a suitable measure of simplicity.-}

import Data.List

data Op = Add | Sub | Mul | Div | Exp 

instance Show Op where 
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y 
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y > 1 && x `mod` y == 0 --had to change this to y>1 because got crashes due to divide by zero once Exp was added
valid Exp x y = x > 1 && y > 1 

apply :: Op -> Int -> Int -> Int 
apply Add x y = x + y 
apply Sub x y = x - y 
apply Mul x y = x * y 
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where 
    show (Val n) = show n 
    show (App o l r) = brak l ++ show o ++ brak r 
                        where 
                            brak (Val n) = show n 
                            brak e = "(" ++ show e ++ ")"

countOps :: Expr -> Int
countOps (Val n) = 0
countOps (App o l r) = 1 + (countOps l) + (countOps r)

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> (Op -> Int -> Int -> Bool) -> [Int]
eval (Val n) check = [n | n > 0]
eval (App o l r) check = [apply o x y | x <- eval l check,
                                  y <- eval r check,
                                  check o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss 
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls,rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
                    lx <- results ls,
                    ry <- results rs,
                    res <- combine lx ry]

combine :: Result -> Result -> [Result]
combine (l,x) (r,y) = 
    [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = 
    [e | ns' <- choices ns, (e,m) <- results ns', m == n]

solutions' :: [Int] -> Int -> (Int, [Expr])
solutions' ns n = 
    (offset, sortedsolutions)
        where
            sortedsolutions = sortBy (\x y -> compare (countOps x) (countOps y)) unsortedsolutions
            unsortedsolutions = solutionlist !! offset
            offset = length (takeWhile null solutionlist)
            solutionlist = [[e | (e,v) <- allsolutions, v == offset] | offset <- [0..]]
            allsolutions = [(e, abs (m-n)) | ns' <- choices ns, (e,m) <- results ns']

ns = [1,3,7,10,25,50]
match1 = 765 
sols1 = solutions' ns match1 
match2 = 831
sols2 = solutions' ns match2

main :: IO ()
main = do 
    putStrLn "Solution to exercise 6."
    putStrLn "----------------------------------------------------"
    putStrLn "The offset of a solution is 0 if the solution is exact.  Otherwise, it is a positive integer representing the total difference from the desired value."
    putStrLn "Solutions are sorted from the fewest number of operations to the most."
    putStrLn "If an exact set of solutions does not exist, then the first non-zero offset that contains at least 1 solution will be returned with every solutions that shares the same offset."
    putStrLn "----------------------------------------------------"
    putStrLn ("Solutions over list " ++ (show ns) ++ " matching " ++ (show match1) ++ " are " ++ (show (snd sols1)) ++ " with an offset of " ++ (show (fst sols1)))
    putStrLn ""
    putStrLn ("Solutions over list " ++ (show ns) ++ " matching " ++ (show match2) ++ " are " ++ (show (snd sols2)) ++ " with an offset of " ++ (show (fst sols2)))

