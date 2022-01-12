-- 8.1 Type declarations
type Pair a = (a,a)
type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v 
find k t = head [v | (k',v) <- t, k == k']

-- 8.4 Recursive types
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int 
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n 

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

-- add function with conversion
-- add :: Nat -> Nat -> Nat
-- add m n = int2nat (nat2int m + nat2int n)

-- add function with recursion
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

-- binary tree with recursion
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
            (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- 8.6 Tautology checker
{-extended in exercise 8 with other propositions
data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Imply Prop Prop 
-}
type Subst = Assoc Char Bool

{-extended in exercise 8 with other propositions
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b 
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p 
vars (And p q) = vars p ++ vars q 
vars (Imply p q) = vars p ++ vars q
-}

bools :: Int -> [[Bool]]
{-can be replaced with recursive definition
bools n = map (reverse . map conv . make n . int2bin) range
            where
                range = [0..(2^n)-1]
                make n bs = take n (bs ++ repeat 0)
                conv 0 = False
                conv 1 = True
-}
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss 
            where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups  (vars p)

-- rmdups from chapter 7
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

-- 8.7 Abstract machine
{- extended in exercize 9 to include multiplication
data Expr = Val Int | Add Expr Expr

{- superceeded below with control stack
value :: Expr -> Int
value (Val n) = n 
value (Add x y) = value x + value y 
-}

type Cont = [Op]

data Op = EVAL Expr | ADD Int 

eval' :: Expr -> Cont -> Int 
eval' (Val n) c = exec c n 
eval' (Add x y) c = eval' x (EVAL y : c)

exec :: Cont -> Int -> Int 
exec [] n = n 
exec (EVAL y : c) n = eval' y (ADD n : c)
exec (ADD n : c) m = exec c (n+m)

value :: Expr -> Int
value e = eval' e [] 
-}
-- 8.9 Exercises
{- 1. In a similar manner to the function add, define a recursive multiplication function mult :: Nat -> Nat -> Nat for the recursive type of natrual numbers:
Hint: make use of add in your definition-}

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ Zero) n = n
mult (Succ m) n = (add n (mult m n))

onetest = nat2int (mult (int2nat 3) (int2nat 9)) == 27

{- 2. Although not included in appendix B, the standard prelude defines
data Ordering = LT | EQ | GT
together with a function
compare :: Ord a => a -> a -> Ordering
that decides if one value in an ordered type is less than (LT), equal to (EQ), or greater than (GT) another value.  Using this function, redefine the function 
occurs :: Ord a => a -> Tree a -> Bool
for search trees.  Why is this new definition more efficient than the original version?-}

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y 
occurs' x (Node l y r) | comp == EQ = True
                       | comp == LT = occurs x l 
                       | otherwise = occurs x r 
                          where comp = compare x y

-- this version only checks the ordering between x and y once and then confirms the value.  We still have to check for the particular value of the comparison but presumably that is easier than the equality relations of numbers 

-- could also write this with a case expression so we can use pattern matching within the function body 
occurs'' x (Node l y r) = case compare x y of 
                            LT -> occurs x l 
                            EQ -> True 
                            GT -> occurs x r 
-- I'm not sure if after compilation the case version is equally efficient to the one with guards


{- 3. Consider the following type of binary trees:
data Tree a = Leaf a | Node (Tree a) (Tree a)
Let us say that such a tree is balanced if the number of leaves in the left and right subtree of every node differ by at most one,
with leaves themselves being trivially balanced.  Define a function balanced :: Tree a -> Bool that decides if a binary tree is balanced or not.
Hint: first define a function that returns the number of leaves in a tree. -}

data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a) deriving Show

countLeaves :: Tree' a -> Int
countLeaves (Leaf' x) = 1 
countLeaves (Node' l r) = countLeaves l + countLeaves r

balanced :: Tree' a -> Bool
balanced (Leaf' x) = True 
balanced (Node' l r) | abs ((countLeaves l) - (countLeaves r)) <= 1 && balanced l && balanced r = True
                    | otherwise = False

{- 4. Define a function balance :: [a] -> Tree a that converts a non-empty list into a balanced tree.  
Hint: first define a function that splits a list into two halves whose length differs by at most one.-}

split :: [a] -> ([a], [a])
split [] = ([], [])
split xs = (take n xs, drop n xs) where n = (length xs) `div` 2

balance :: [a] -> Tree' a
balance [] = error "Cannot balance an empty list"
balance (x:[]) = Leaf' x
balance xs = Node' l r
    where 
        l = balance (fst splt)
        r = balance (snd splt)
        splt = split xs

fourcheck = balanced (balance [1,2,3,4,5])

{- 5. Given the type declaration
data Expr = Val Int || Add Expr Expr
define a higher-order function 
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a 
such that folde f g replaces each Val constructor in an expression by the function f, and each Add constructor by the function g.-}

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a 
folde f _ (Val a) = f a 
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)  

{- 6. Using folde, define a function eval :: Expr -> Int that evaluates an expression to an integer value, and a function size :: Expr -> Int 
that calculates the number of values in an expression-}

eval'' :: Expr -> Int
eval'' e = folde id g e 
    where g = \x y -> x + y

sixcheck = eval'' (Add (Val 5) (Val 6)) == 11

{- 7. Complete the following instance declarations 
instance Eq a => Eq (Maybe a) where
...
instance Eq a => Eq [a] where 
...
-}

{- commented out because of conflicts with existing instance declarations
instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    Just a == Just b = a == b
    Nothing == _ = False 
    _ == Nothing = False


instance Eq a => Eq [a] where 
    [] == [] = True 
    _ == [] = False
    [] == _ = False
    (x:xs) == (y:ys) = x == y && xs == ys
-}

{- 8. Extend the tautology checker to support the use of logical discuntion and equivalance in propositions-}

data Prop = Const Bool
            | Var Char
            | Not Prop
            | Or Prop Prop
            | And Prop Prop
            | Imply Prop Prop 
            | Equiv Prop Prop

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b 
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q) = eval s p || eval s q 
eval s (Equiv p q) = eval s p == eval s q  

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p 
vars (And p q) = vars p ++ vars q 
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q 
vars (Equiv p q) = vars p ++ vars q

ptest :: Prop
ptest = Equiv (Or (Var 'A') (Not (Var 'A'))) (And (Var 'A') (Var 'A'))
eightcheck = isTaut ptest

-- 9. Extend the abstract machine to support the use of multiplication.
data Expr = Val Int | Add Expr Expr | Mult Expr Expr

type Cont = [Op]

data Op = EVAL_ADD Expr | EVAL_MULT Expr | ADD Int | MULT Int

eval' :: Expr -> Cont -> Int 
eval' (Val n) c = exec c n 
eval' (Add x y) c = eval' x (EVAL_ADD y : c)
eval' (Mult x y) c = eval' x (EVAL_MULT y : c)

exec :: Cont -> Int -> Int 
exec [] n = n 
exec (EVAL_ADD y : c) n = eval' y (ADD n : c)
exec (EVAL_MULT y : c) n = eval' y (MULT n : c)
exec (ADD n : c) m = exec c (n+m)
exec (MULT n : c) m = exec c (n*m)

value :: Expr -> Int
value e = eval' e [] 

testexpr = Add (Mult (Val 6) (Val 5)) (Val 13)
ninecheck = (value testexpr) == (6 * 5) + 13