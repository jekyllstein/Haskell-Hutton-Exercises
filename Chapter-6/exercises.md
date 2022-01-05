> 1. How does the recursive version of the factorial function behave if applied to a negative argument, such as (-1)?  Modify the definition to prohibit negative arguments by adding a guard to the recursive case.

The factorial function defined in chapter 6:
```haskell
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)
```

If we pass (-1) into this function, the following calculations occur:
$$
\text{fac} -1 = -1 * \text{fac} (-2)
= -1 * -2 * \text{fac} (-3) = \cdots
$$
and the recursion will never end.  To fix this we can throw an error if a negative argument is passed.
```haskell
fac n | n > 0 = n * fac (n-1)
      | otherwise = error "fac does not accept negative numbers"
```
> 2. Define a recursive function `sumdown :: Int -> Int` that returns the sum of the non-negative integers from a given value down to zero.  For example, `sumdown 3` should return the result `3+2+1+0=6`.

```haskell
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n - 1)
          | otherwise = error "sumdown not defined for negative arguments"
```
> 3. Define the exponentiation operator `^` for non-negative integers using the same pattern of recursion as the multiplication operator `*`, and show how the expression `2 ^ 3` is evaluated using your definition

```haskell
(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n | (m >= 0) && (n >= 0) = m * (m ^ (n - 1))
      | otherwise = error "^ not defined for negative arguments"
```

```
2 ^ 3 = 2 * (2 ^ 2) = 2 * (2 * (2 ^ 1)) = 2 * (2 * (2 * (2 ^ 0))) = 2 * 2 * 2 * 1 = 8
```

> 4. Define a recursive function `euclid :: Int -> Int -> Int` that implements *Euclid's algorithm* for calculating the greatest common divisor of two non-negative integers: if the two numbers are equal, this number is the result; otherwise, the smaller number is subtracted from the larger, and the same process is then repeated.  For example:
> ```
> > euclid 6 27
> 3
> ```

```haskell
euclid :: Int -> Int -> Int
euclid n m | n == m = n
           | n < m = euclid (m - n) n
           | n > m = euclid (n - m) m
```

> 5. Using the recursive definitions given in this chapter, show how `length [1,2,3]`, `drop 3 [1,2,3,4,5]`, and `init [1,2,3]` are evaluated.

```
length [1,2,3] = 1 + length [2,3] = 1 + (1 + length [3]) = 1 + (1 + (1 + length [])) = 3
drop 3 [1,2,3,4,5] = drop 2 [2,3,4,5] = drop 1 [3,4,5] = drop 0 [4,5] = [4,5]
init [1,2,3] = 1 : init [2,3] = 1 : 2 : init [3] = 1 : 2 : [] = [1,2]
```

> 6. Without looking at the definitions from the standard prelude, define the following library functions on lists using recursion.

> a. Decide if all logical values in a list are `True`:
```haskell
and :: [Bool] -> Bool
and [] = False
and (False:_) = False
and (True:[]) = True
and (True:xs) = and xs
```
> b. Concatenate a list of lists:

```haskell
concat :: [[a]] -> [a]
concat (xs:[]) = xs 
concat ([]:xss) = concat xss
concat ((x:xs):xss) = x : concat (xs:xss)
``` 
> c. Produce a list with n identical elements:
```haskell
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a = a : replicate (n - 1) a
```

> d. Select the nth element of a list:

```haskell
(!!) :: [a] -> Int -> a
[] !! n = error "index out of range"
(x:_) !! 0 = x
(x:xs) !! n = xs !! (n - 1)
```

> e. Decide if a value is an element of a list:

```haskell
elem :: Eq a => a -> [a] -> Bool
elem a [] = False
elem a (x:xs) | a == x = True
              | otherwise = elem a xs
```

> 7. Define a recursive function `merge :: Ord a => [a] -> [a] -> [a]` that merges two sorted lists to give a single sorted list. For example:
> ```
> > merge [2,5,6] [1,3,4]
> [1,2,3,4,5,6]
> ```
> Note: your definition should not use other functions on sorted lists such as `insert` or `isort`, but should be defined using explicit recursion.

```haskell
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge allx@(x:xs) ally@(y:ys) | x < y = x : merge xs ally 
                              | y < x = y : merge allx ys
                              | otherwise = x : y : merge xs ys
```

> 8. Using `merge` define a function `msort :: Ord a => [a] -> [a]` that implements *merge sort*, in which the empty list and singleton lists are already sorted, and any other list is sorted by merging together the two lists that result from sorting the two halves of the list separately.

>Hint: first define a function `halve :: [a] -> ([a],[a])` that splits a list into two haves whose lengths differ by at most one.
```haskell
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs) where n = div (length xs) 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort (a:[]) = [a]
msort xs = merge (msort (fst split)) (msort (snd split)) where split = halve xs
```

> 9. Using the five-step process, construct library functions that:

>a. calculate the `sum` of a list of numbers;
```haskell
-- Step 1: define the type
sum :: Num a => [a] -> a

-- Step 2: enumerate the cases, Step 3: define the simple cases, Step 4: define the other cases
sum [] = 0
sum (x:xs) = x + sum xs

-- Step 5: generalize and simplify, not needed because already generalized to any number

```
>b. `take` a given number of elements from the start of a list;
```haskell
-- Step 1: define the type
take :: Int -> [a] -> [a]

-- Step 2: enumerate the cases, Step 3: define the simple cases, Step 4: define the other cases
take _ []       = [] 
take 0 _        = []
take n (x:xs)   = x : take (n-1) xs

-- Step 5: generalize and simplify
take :: Integral b => b -> [a] -> [a]
```
>c. select the `last` element of a non-empty list.
```haskell
-- Step 1: define the type
last :: [a] -> a 

-- Step 2: enumerate the cases, Step 3: define the simple cases, Step 4: define the other cases
last []       = error "cannot take the last element of an empty list" 
last (x:[])   = x 
last (x:xs)   = last xs 

-- Step 5: generalize and simplify, not needed
```