import Data.Char
import Data.List

-- 7.6 Binary string transmitter
type Bit = Int

bin2int :: [Bit] -> Int
{- can be implified
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
                   where weights = iterate (*2) 1
-}
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- 7.7 Voting Algorithms
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a 
winner = snd . last . result

ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a] 
rank = map snd . result . map head 

winner' :: Ord a => [[a]] -> a 
winner' bs = case rank (rmempty bs) of 
                [c] -> c 
                (c:cs) -> winner' (elim c bs)

-- Exercise 7. modifications to binary string transmitter

addparity :: [Bit] -> [Bit]
addparity bits | odd (sum bits) = bits ++ [1]
               | otherwise = bits ++ [0]

encode' :: String -> [Bit]
encode' = concat . map (addparity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkparity :: [Bit] -> [Bit]
checkparity bitss | even s && (p == 0) = bits
                  | odd s && (p == 1)  = bits
                  | otherwise = error "parity check error"
                  where 
                    bits = take 8 bitss
                    s = sum bits
                    p = last bitss

decode' :: [Bit] -> String
decode' = map (chr . bin2int . checkparity) . chop9

transmit' :: String -> String
transmit' = decode' . channel . encode'

-- Exercise 8. test with faulty channel that forgets the first bit 
badchannel :: [Bit] -> [Bit]
badchannel = tail 

faultytransmit :: String -> String
faultytransmit = decode' . badchannel . encode'

testout = faultytransmit "test string"
{-
> testout 
"*** Exception: parity check error..."
-}

-- Exercise 0. Luhn algorithm using altMap
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = (f x) : altMap g f xs

-- from chapter 4, implements the operation on every digit starting second to last
luhnDouble :: Int -> Int
luhnDouble x | y > 9 = y - 9
             | otherwise = y
             where y = 2*x

-- Luhn algorithm from chapter 4 that only works on 4 digits
shortluhn :: Int -> Int -> Int -> Int -> Bool
shortluhn x1 x2 x3 x4 = tot `mod` 10 == 0
    where tot = (luhnDouble x1) + x2 + (luhnDouble x3) + x4 

-- Luhn algorithm using altMap that works on any length number
luhn :: [Int] -> Bool
luhn [] = False
luhn xs = tot `mod` 10 == 0
    where 
        tot = sum xs'
        xs' = altMap id luhnDouble (reverse xs)
