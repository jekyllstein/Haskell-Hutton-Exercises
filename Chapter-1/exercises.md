> 1. Give another possible calculation for the result of `double (double 2)`

```haskell 
double x = x + x
```
`double (double 2)`

= {applying outer double}

`(double 2) + (double 2)`

= {applying second double}

`(double 2) + (2+2)`

= {applying second +}

`(double 2) + 4`

= {applying double}

`(2+2) + 4`

= {applying first +}

`4 + 4`

= {applying +}

`8`

> 2. Show that `sum [x] = x` for any number x.

```haskell
sum [] = 0
sum (n:ns) = n + sum ns
```
Since `[x] = x:[]` we can use the second part of the definition yielding `sum (x:[]) = x + sum [] = x + 0 = x`

> 3. Define a function `product` that produces the product of a list of numbers, and show using your definition that `product [2,3,4]=24`

```haskell
product [] = 1
product (x:xs) = x * product xs
```

`product [2,3,4] = 2 * product [3,4] = 2 * (3 * product 4) = 2 * (3 * (4 * 1)) = 2 * (3 * 4) = 2 * 12 = 24`

> 4. How should the definition of the function `qsort` be modified so that it produces a `reverse` sorted version of a list?

Recall the original qsort:
```haskell
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where 
                   smaller = [a | a <- xs, a <= x]
                   larger = [b | b <- xs, b > x]
```

To produce a reversed list, larger should always be appended before smaller around the pivot
```haskell
qsort' [] = []
qsort' (x:xs) = qsort' larger ++ [x] ++ qsort' smaller
               where 
                   smaller = [a | a <- xs, a <= x]
                   larger = [b | b <- xs, b > x]
```

> 5. What would be the effect of replacing <= by < in the original definition of `qsort`?  Hint: consider the example `qsort [2,2,3,1,1].

Any value that equals the pivot will not be included in either `smaller` or `larger` and will be included in the final list.  So the final result will only contain unique values from the original list, rather than every value.  In the example for the question, 2 is the head of the list so the second value of 2 will not appear in the sorted list.  Similarly, only a single 1 will remain from the end of the list so the final result will be `[1,2,3]` instead of `[1,1,2,2,3]`.