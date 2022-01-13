data Op = Add | Sub | Mul | Div 

instance Show Op where 
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y 
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int 
apply Add x y = x + y 
apply Sub x y = x - y 
apply Mul x y = x * y 
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where 
    show (Val n) = show n 
    show (App o l r) = brak l ++ show o ++ brak r 
                        where 
                            brak (Val n) = show n 
                            brak e = "(" ++ show e ++ ")"

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

solution :: Expr -> [Int] -> Int -> Bool 
solution e ns n = 
    elem (values e) (choices ns) && eval e valid == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                 l <- exprs ls,
                 r <- exprs rs,
                 e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = 
    [e | ns' <- choices ns, e <- exprs ns', eval e valid == [n]]

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
                    lx <- results ls,
                    ry <- results rs,
                    res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = 
    [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = 
    [e | ns' <- choices ns, (e,m) <- results ns', m == n]

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y 
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0


-- 1. Redefine the combinatorial function choices using a list comprehension rather than using composition, concat, and map 
choices' :: [a] -> [[a]]
choices' ns = [perm | subset <- subs ns, perm <- perms subset]
onecheck = choices' [1,2,3,4] == choices [1,2,3,4]

{- 2. Define a recursive function isChoice :: Eq a => [a] -> [a] -> Bool that decides if one list is chosen from another, without using the
    combinatorial functions perms and subs.  Hint: start by defining a function that removes the first occurrence of a value from a list-}

--checks to see if the first input could be a permuted subset of the second input.  Keeps trying to removing single instances
-- of values from the first list that appear in the second.  Only returns true if the first list can be emptied out with this method.
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice xs (y:ys) = isChoice (rmValue y xs) ys

rmValue :: Eq a => a -> [a] -> [a]
rmValue _ [] = []
rmValue a (x:xs) | (a == x) = xs
                 | otherwise = x : rmValue a xs

-- 3. What effect would generalizing the function split to also return pairs containing the empty list have on the behavior of solutions?
--modified version of split that includes results with an empty list
split' :: [a] -> [([a],[a])]
split' [] = []
split' [x] = [([x], [])]
split' (x:xs) = ([x], xs) : [(x:ls,rs) | (ls,rs) <- split' xs]
{- 
If split contained a pair with the empty list, the only such pair would be the entire list combined with the empty list.  See the result
of calling split [1,2] vs split' [1,2]
> split [1,2]
[([1],[2])]  
> split' [1,2]
[([1],[2]),([1,2],[])]
-}

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n,n) | n > 0]
results' ns = [res | (ls,rs) <- split' ns,
                    lx <- results' ls,
                    ry <- results' rs,
                    res <- combine' lx ry]
{-
Testing out this modified results function with the list [1,2] first returns the single result we'd expect and then has a stack overflow error from 
infinite recursion.  When the second split is evaluated with the empty list we have lx drawn from results' [1,2] which is the same as the original
function call which will again produce the split that contains [1,2].  Since the function keeps calling itself with the same input, there is no end 
to that particular branch of function evaluation and it will recur forever.
-}

{- 4. Using the functions choices, exprs, and eval, verify that there are 33,665,406 possible expressions over the numbers 1,3,7,10,25,50, and
that only 4,672,540 of these expressions evaluate successfully.-}

fourlist = [1,3,7,10,25,50]
fourchoices = choices fourlist 
fourexprs = concat (map exprs fourchoices)
fournumexprs = length fourexprs
fourevals = filter (not.null) (map (\x -> eval x valid) fourexprs)
fournumeval = length fourevals 
-- Once compiled this will print out the confirmed values for this exercise stored in fournumexprs and fournumeval

{- 5. Similarly, verify that the number of expressions that evaluate successfully increases to 10,839,369 if the numeric domain is generalised to
      arbitrary integers.  Hint: modify the definition of valid-}

validints :: Op -> Int -> Int -> Bool
validints Add _ _ = True 
validints Sub _ _ = True
validints Mul _ _ = True
validints Div x y | y == 0 = False
                  | otherwise = x `mod` y == 0

fivenumeval = length (filter (not.null) (map (\x -> eval x validints) fourexprs))
-- Once compiled this will print out the confirmed values for this exercise stored in fivenumeval. 

main :: IO ()
main = do 
    putStrLn "Solution to exercise 4."
    putStrLn ("Number of expressions over list " ++ (show fourlist) ++ " is " ++ (show fournumexprs))
    putStrLn ("Number of expressions that evaluate successfully is " ++ (show fournumeval))
    putStrLn ""
    putStrLn "Solution to exercise 5."
    putStrLn ("Number of expressions over list " ++ (show fourlist) ++ " that evaluate successfully if we allow arbitrary integers is " ++ (show fivenumeval))

