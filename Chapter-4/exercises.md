> 1. Using library functions, define a function `halve :: [a] -> ([a],[a])` that splits an even-lengthed list into two halves.  For example:
> ```haskell
> > halve [1,2,3,4,5,6]
> ([1,2,3],[4,5,6])

```haskell
halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs) where n = div (length xs)  2
```

> 2. Define a function `third :: [a] -> a` that returns the third element in a list that contains at least this many elements using:

> a. `head` and `tail`;

```haskell
third :: [a] -> a
third xs = head (tail (tail xs))
```
> b. list indexing `!!`;

```haskell
third xs = xs !! 2
```
> c. pattern matching.

```haskell
third (x:y:z:xs) = z
```

> 3. Consider a function `safetail :: [a] -> [a]` that behaves in the same way as `tail` except that it maps the empty list to itself rather than producing an error.  Using `tail` and the function `null :: [a] -> Bool` that decides if a list is empty or not, define `safetail` using:

> a. a conditional expression;

```haskell
safetail xs = if null xs then [] else tail xs
``` 

> b. guarded equations;

```haskell
safetail xs | null xs = []
            | otherwise = tail xs
```

> c. pattern matching.

```haskell
safetail [] = []
safetail xs = tail xs
```

> 4. In a similar way to `&&` in section 4.4, show how the disjunction operator `||` can be defined in four different ways using pattern matching.

version 1
```haskell
(||) :: Bool -> Bool -> Bool
False || False = False
True || True = True
True || False = True
False || True = True
```

version 2
```haskell
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True
```
version 3
```haskell
(||) :: Bool -> Bool -> Bool
_ || True = True
b || False = b 
```
version 4
```haskell
(||) :: Bool -> Bool -> Bool
b || c | b /= c = True
       | otherwise = b
```

> 5. Without using any other library functions or operators, show how the meaning of the following pattern matching definition for logical conjunction `&&` can be formalized using conditional expressions:

> ```haskell
> True && True = True
> _    && _    = False
> ```
> Hint: use two nested conditional expressions
```haskell
a && b = if a then
    if b then True else False
    else
        False
```

> 6. Do the same for the following alternative definition, and note the difference in the number of conditional expressions that are required:

> ```haskell
> True && b   = b
> False && _  = False
> ```

```haskell
a && b = if a then b else False
```

> 7. Show how the meaning of the following curried function definition can be formalized in terms of lambda expressions:

> ```haskell
> mult :: Int -> Int -> Int -> Int
> mult x y z = x*y*z
> ```

```haskell
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x*y*z))
```

> 8. The *Luhn algorithm* is used to check bank card numbers for simple errors such as mistyping a digit, and proceeds as follows:
> - consider each digit as a separate number;
> - moving left, double every other number from the second last;
> - subtract 9 from each number that is now greater than 9;
> - add all the resulting numbers together;
> - if the total is divisible by 10, the card number is valid.

> Define a function `luhnDouble :: Int -> Int` that doubles a digit and subtracts 9 if the result is greater than 9.  For example:
> ```
> > luhnDouble 3
> 6
> > luhnDouble 6
> 3
> ```
> Using `luhnDouble` and the integer remainder function `mod`, define a function `luhn :: Int -> Int -> Int -> Int -> Bool` that decides if a four-digit bank card number is valid.  For example:
> ```
> > luhn 1 7 8 4
> True
> > luhn 4 7 8 3
> False
> ```
> In the exercises for chapter 7 we will consider a more general version of this function that accepts card numbers of any length.

```haskell
luhnDouble :: Int -> Int
luhnDouble x | y > 9 = y - 9
             | otherwise = y
             where y = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x1 x2 x3 x4 = tot `mod` 10 == 0
    where tot = (luhnDouble x1) + x2 + (luhnDouble x3) + x4 
```

