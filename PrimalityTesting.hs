module PrimalityTesting
( isPrimeTrialDivide
, fermatPseudoPrime
, absolutePseudoPrimeTest
, getPseudoPrimes
, basesToCheck
, hcf
, jacobi
, eulerPseudoPrime
, strongBaseTest
) where

import Data.List

dec2bin:: Integer -> [Integer] -- e.g. dec2bin 13 = [3, 2, 0] (i.e. 2^3 + 2^2 + 2^0)
dec2bin 0 = []
dec2bin 1 = [0] 
dec2bin x =
    let y = floor $ (log $ fromInteger x) / log 2
    in y : (dec2bin (x-2^y))

-- squareModSeq a 4 N = [a, a^2, a^4, a^8] mod N
squareModSeq::Integer -> Integer -> Integer -> [Integer] 
squareModSeq a pow2 c = scanl (\acc x -> acc^2 `mod` c) a [0..pow2-1]


powerMod::Integer -> Integer -> Integer ->Integer -- calculates a^b mod c efficiently
powerMod a b c = toInteger $ foldl (\acc x -> acc*a_powers!!x `mod` c) 1 b_bin
    where
        b_bin = map fromInteger (dec2bin b)
        a_powers = squareModSeq a (toInteger $ maximum b_bin) c



isPrimeTrialDivide::Integer -> Bool -- True/False if input is prime
isPrimeTrialDivide 1 = False
isPrimeTrialDivide 2 = True
isPrimeTrialDivide x = not $ any (\y -> x `mod` y == 0) [2..ceiling $ sqrt $ fromInteger x]


fermatPseudoPrime::Integer -> Integer -> Bool -- T/F if n is a fermat pseudoprime base a
fermatPseudoPrime n a = powerMod a (n-1) n == 1 --a^(n-1) `mod` n == 1


-- absolute-pseudoprimeven a non-prime which passes pseudoPrimeTest to every (a, N)=1 basis
absolutePseudoPrimeTest:: (Integer -> Integer -> Bool) -> (Integer -> Bool)
absolutePseudoPrimeTest pseudoPrimeTest n = (not . isPrimeTrialDivide) n &&
    (all (pseudoPrimeTest n) $ filter (\x -> (hcf n x) == 1) [1..ceiling $ sqrt $ fromInteger (n-1)])


-- selects subset of nums in test_list which pass pseudo_prime_test wrt base (and aren't prime)
getPseudoPrimes:: (Integer -> Integer -> Bool) -> [Integer] -> [Integer] -> [Integer]
getPseudoPrimes pseudo_prime_test bases test_list = filter (not . isPrimeTrialDivide) passed_PP_test
    where   isPP n =  all (\base -> hcf n base == 1 && pseudo_prime_test n base) bases
            passed_PP_test = filter isPP test_list


-- gives the number of bases checked to determine primality/compositeness of n.  -1 if n is prime
basesToCheck:: (Integer -> Integer -> Bool) -> Integer -> [Integer] 
basesToCheck pseudo_prime_test n = [n, first_index]
    where 
        checkBase base = (hcf n base == 1) && (pseudo_prime_test n base)
        tests = map checkBase (filter isPrimeTrialDivide [2..ceiling $ sqrt $ fromInteger n])
        first_index = case elemIndex False tests of Just i -> toInteger (i+1); Nothing -> -1



getPrimesLeq:: Integer -> [Integer] -- returns all primes 2,3,... less than n
getPrimesLeq 1 = []
getPrimesLeq 2 = [2]
getPrimesLeq n =
    let plist = getPrimesLeq (n-1)
        f x numlist = any (\y -> x `mod` y == 0) (filter (< (ceiling $ sqrt $ fromInteger n)) numlist)
    in if f n plist then plist else plist ++ [n]


-- rearranges s.t. a > b > 0 and then returns [b, a - p*b = r] with b > r >= 0 
eulerStep::Integer -> Integer -> [Integer] 
eulerStep a b = 
    let x=minimum [a,b]; y=maximum [a,b]; p = floor (fromInteger y/ fromInteger x)
    in [x, y - p*x]

hcf::Integer -> Integer -> Integer -- highest common factor
hcf a 0 = a
hcf 0 b = b
hcf a 1 = 1
hcf 1 b = 1
hcf a b = let [x,y] = eulerStep a b in hcf x y


-- 0, 1 or -1. m should be odd. is product of legendre symbols m on p_i
jacobi::Integer -> Integer -> Integer 
jacobi 1 n = 1
jacobi 0 n = 0
jacobi (-1) n = if n `mod` 4 == 1 then 1 else -1
jacobi 2 n = let t = n `mod` 8 in if t `elem` [1,7] then 1 else -1
jacobi m n  
    | m `mod` 2 == 0    = (jacobi 2 n) * (jacobi (m `div` 2) n)
    | m < 0             = (jacobi (-1) n) * (jacobi (-m) n)
    | m >= n            = jacobi (m `mod` n) n
    | m < n             = (if m `mod` 4 == 1 || n `mod` 4 == 1  then 1 else -1) * jacobi n m

--Note a `mod` 2 == 1 is important for (n-1)/2 : e.g. n = 100000034, a = 3
eulerPseudoPrime::Integer -> Integer -> Bool
eulerPseudoPrime n a
    | n `mod` 2 == 0    = False
    | otherwise         = (jacobi a n) `mod` n == powerMod a ((n-1) `div` 2) n

stripEven::Integer -> (Integer, Integer)
stripEven n
    | n == 0            = (0, 1)
    | n `mod` 2 /= 0    = (0, n)
    | n `mod` 2 == 0    = let(x,y) = stripEven (n `div` 2) in (x+1, y)


strongBaseTest::Integer -> Integer -> Bool
strongBaseTest n a = powerMod a s n == 1 ||
    any (\x -> x == (-1) `mod` n) [powerMod a (s*2^k) n | k <- [0..(r-1)]]
    where (r, s) = stripEven (n-1) -- n-1 = s * 2^r




