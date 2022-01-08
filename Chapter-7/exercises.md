> 1. Show how the list comprehension `[f x | x <- xs, p x]` can be re-expressed using the higher-order functions `map` and `filter`.

```haskell
filter p map f x
```
> 2. Without looking at the definitions from the standard prelude, define the following higher-order library functions on lists.

> a. Decide if all elements of a list satisfy a predicate:

```haskell
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p 
```

> b. Decide if any element of a list satisfies a predicate:

```haskell
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p
```

> c. Select elements from a list while they satisfy a predicate:

```haskell
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) | p x = x : takeWhile p xs
                   | otherwise = []
```
> d. Remove elements from a list while they satisfy a predicate:

```haskell
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p allx@(x:xs) | p x = dropWhile p xs
                   | otherwise = allx 
```

> 3. Redefine the functions `map f` and `filter p` using `foldr`.

```Haskell
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> (f x):xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr f [] 
    where f x xs | p x = x:xs
                 | otherwise = xs
```

> 4. Using `foldl`, define a function `dec2int :: [Int] -> Int` that converts a decimal number into an integer.  For example:
> ```
> > dec2int [2,3,4,5]
> 2345
> ```

```haskell
dec2Int :: [Int] -> Int
dec2Int = foldl (\x y -> 10*x + y) 0 
```
> 5. Without looking at the definitions from the standard prelude, define the higher-order library function `curry` that converts a function on pairs into a curried function, and, conversely, the function `uncurry` that converts a curried function with two arguments into a function on pairs.

> Hint: first write down the types of the two functions.

```haskell
curry :: ((a,b) -> c) -> a -> b -> c
curry f = \x -> (\y -> f (x,y))

uncurry:: (a -> b -> c) -> (a,b) -> c
uncurry f = \(x,y) -> f x y
```
> 6. A higher-order function `unfold` that encapsulates a simple pattern of recursion for producing a list can be defined as follows:
> ```haskell
> unfold p h t x | p x        = []
>                | otherwise  = h x : unfold p h t (t x)
> ```
> That is, the function `unfold p h t` produces the empty list if the predicate `p` is true of the argument value, and otherwise produces a non-empty list by applying the function `h` to this value to give the head, and the function `t` to generate another argument that is recursively processed in the same way to produce the tail of the list.  For example, the function `int2bin` can be rewritten more compactly using `unfold` as follows:
> ```haskell
> int2bin = unfold (== 0) ('mod' 2) ('div' 2)
> ```
> Redefine the functions `chop8`, `map f`, and `iterate f` using `unfold`. 

```haskell
chop8 = unfold null (take 8) (drop 8)

map f = unfold null (f . head) tail

iterate f = unfold (\_ -> False) id f 
```

> 7. Modify the binary string transmitter example to detect simple transmission errors using the concept of parity bits.  That is, each eight-bit binary number produced during encoding is extended with a parity bit, set to one if the number contains an odd number of ones, and to zero otherwise.  In turn, each resulting nine-bit binary number consumed during decoding is checked to ensure that its parity bit is correct with the parity bit being discarded if this is the case, and a parity bit error being reported otherwise.

>Hint the library function `error :: String -> a` displays the given string as an error message and terminates the program; the polymorphic result type ensures that `error` can be used in any context.

See solution in sample_code.hs

> 8. Test your new string transmitter program from the previous exercise using a faulty communication channel that forgets the first bit, which can be modelled using the `tail` function on lists of bits.

See solution in sample_code.hs

> 9. Define a function `altMap :: (a -> b) -> (a -> b) -> [a] -> [b]` that alternately applies its two argument functions to successive elements in a list, in turn about order.  For example
> ```
> > altMap (+10) (+100) [0,1,2,3,4]
> [10,101,12,103,14]

```Haskell
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = (f x) : altMap g f xs
```

> 0. Using `altMap`, define a function `luhn :: [Int] -> Bool` that implements the *Luhn algorithm* for the exercises in chapter 4 for bank card numbers of any length.  Thest your new function using your own bank card.

See solution in sample_code.hs