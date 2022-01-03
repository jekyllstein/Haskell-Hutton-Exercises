> 1. What are the types of the following values?
> ```haskell
> ['a', 'b', 'c']
> ```
```haskell
:: [Char]
```
> ```haskell
> ('a', 'b', 'c')
> ```

```haskell
:: (Char, Char, Char) 
```

> ```haskell
> [(False, '0'), (True, '1')]
> ```

```haskell
:: [(Bool, Char)]
```
> ```haskell
> ([False, True], ['0', '1'])
> ```

```haskell
:: ([Bool], [Char])
```

> ```haskell
> [tail, init, reverse]
> ```

```haskell
[[a] -> [a]]
```

> 2. Write down definitions that have the following types; it does not matter what the definitions actually do as long as they are type correct.
> ```haskell
> bools:: [Bool]
> ``` 

```haskell
bools = [True, False]
```
>  ```haskell
> nums :: [[Int]]
> ```

```haskell
nums = [[1,2],[2,3,4]]
```

> ```haskell
> add :: Int -> Int -> Int -> Int
> ```


```haskell
add x y z = x + y + z
```


> ```haskell
> copy :: a -> (a,a)
> ```


```haskell
copy a = (a,a)
```

> ```haskell
> apply :: (a -> b) -> a -> b
> ```


```haskell
apply f a = f a
```

> 3. What are the types of the following functions?

> ```haskell
> second xs = head (tail xs)
> ```


```haskell
:: [a] -> a
```


> ```haskell
> swap (x,y) = (y,x)
> ```


```haskell
:: (a,b) -> (b,a)
```


> ```haskell
> pair x y = (x,y)
> ```



```haskell
:: a -> b -> (a,b)
```

> ```haskell
> double x = x*2
> ```


```haskell
:: Num a => a -> a
```

> ```haskell
> palindrome xs = reverse xs == xs
> ```


```haskell
:: Eq a => [a] -> bool
```

> ```haskell
> twice f x = f (f x)
> ```

```haskell
:: (a -> a) -> a -> a
```


> 5. Why is it not feasible in general for function types to be instances of the `Eq` class?  When is it feasible?  Hint: two functions of the same type are equal if they always return equal results for equal arguments.

The compiler would not know the output of a function prior to the arguments being known.  If a function was written such that it always returned a constant value that did not depend on the input values, then it might be possible to infer that two functions are equal as long as it can see the return value from the code itself.  If the body of two functions is identical to the parser, then it might also be possible to consider them equal, if the compiler has a way of checking that two parsed functions were written identically.