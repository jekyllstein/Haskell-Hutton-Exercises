double x = x + x

quadruple x = double (double x)

-- Factorial of a positive integer:
factorial n = product [1..n]

-- Average of a list of integers:
average ns = sum ns `div` length ns

a = b + c 
     where
        b = 1
        c = 2
d = a * 2

{-
2. Parenthesise the following numeric experessions:

    2^3*4
    2*3+4*5
    2+3*4^5
-}

check1 = (2^3)*4 == 2^3*4 
-- True
check2 = (2*3)+(4*5) == 2*3+4*5 
-- True
check3 = 2+(3*(4^5)) == 2+3*4^5 
-- True

{-
3. The script below contains three syntactic errors.  Correct these errors and then check that your script works properly using GHCi.

N = a `div` length xs
    where

        a = 10
       xs = [1,2,3,4,5]
-}

-- n fixed to not capital, extra line removed after where, a and xs indented the same
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]


{-
4. The library function last selects the last element of a non-empty list; for example, last [1,2,3,4] = 5.  Show how the function last could be defined in terms of other library functions introduced in this chapter.  Can you think of another possible definition?
-}

-- get the last element of a list with indexing
newlast1 xs = xs !! (length xs - 1)

-- alternatively, we can define last recursively with pattern matching

-- pattern matching for a list with only 1 element to just return that element, note that this function will fail in the case of an empty list since the return type for that would have to be different than a list element
newlast2 (x:[]) = x
newlast2 (x:xs) = newlast2 xs

{-
5. The library function init removes the last element from a non-empty list; for example, init [1,2,3,4,5] = [1,2,3,4].  Show how init could similarly be defined in two different ways.
-}

-- take all but the last element of a list
newinit1 xs = take (length xs - 1) xs

-- recursively form a new list appending each element except the final one
newinit2 (x:[]) = []
newinit2 (x:xs) = [x] ++ newinit2 xs 