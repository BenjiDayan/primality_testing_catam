import qualified Data.Set
import qualified Data.List.Ordered
import Data.List
import Data.List.Split


dec2bin:: Integer -> [Integer] -- e.g. dec2bin 13 = [3, 2, 0] (i.e. 2^3 + 2^2 + 2^0)
dec2bin 0 = []
dec2bin 1 = [0] 
dec2bin x =
    let
        y = floor $ (log $ fromInteger x) / log 2

    in y : (dec2bin (x-2^y))


squareModSeq::Integer -> Integer -> Integer -> [Integer] -- squareModSeq a 4 N = [a, a^2, a^4, a^8] mod N
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

q1_getPrimes::Integer -> Integer -> [Integer] -- Returns list of primes in [lower bound, upper bound]
q1_getPrimes lb ub = filter isPrimeTrialDivide [lb..ub]

q1_answers = [q1_getPrimes 1900 1999, q1_getPrimes 1294268500 1294268700]

fermatPseudoPrime::Integer -> Integer -> Bool -- T/F if n is a fermat pseudoprime base a
fermatPseudoPrime n a = powerMod a (n-1) n == 1 --a^(n-1) `mod` n == 1




data NumPrimeBase = NumPrimeBase Integer Bool [Integer]
    deriving Show

q2_answers = [
    filter is_PP_some_base [NumPrimeBase n (isPrimeTrialDivide n)  (filter (fermatPseudoPrime n) [1..13]) | n <- range]
    | range <- [[1900..1999],[1294268500..1294268700]]
    ]
    where is_PP_some_base x = case x of NumPrimeBase _ False [1] -> False; _ -> True


-- generates a function which returns T/F if its input is a non-prime which passes pseudoPrimeTest to every basis {}
absolutePseudoPrimeTest:: (Integer -> Integer -> Bool) -> (Integer -> Bool)
absolutePseudoPrimeTest pseudoPrimeTest n = (not . isPrimeTrialDivide) n &&
    (all (pseudoPrimeTest n) $ filter (\x -> (hcf n x) == 1) [1..n-1])

absolutePseudoPrimeTest2:: (Integer -> Integer -> Bool) -> (Integer -> Bool) --hypothetically might be faster than absolutePseudoPrimeTest? not sure
absolutePseudoPrimeTest2 pseudoPrimeTest n = 
    (all (\pair -> pseudoPrimeTest n $ pair!!0) $ filter (\pair -> pair!!1 == 1) hcflist) &&
    any (\pair -> pair!!1 /= 1) hcflist
    where hcflist = [[x, hcf n x] | x <- [1..n-1]]


-- selects subset of nums in test_list which pass pseudo_prime_test wrt base (and aren't prime)
getPseudoPrimes:: (Integer -> Integer -> Bool) -> [Integer] -> [Integer] -> [Integer]
getPseudoPrimes pseudo_prime_test bases test_list = filter (not . isPrimeTrialDivide) passed_PP_test
    where passed_PP_test = filter (\n -> all (pseudo_prime_test n) bases) test_list

basesToCheck:: (Integer -> Integer -> Bool) -> Integer -> [Integer] -- gives the number of bases checked to determine primality/compositeness of n. Returns -1 if n is prime
basesToCheck pseudo_prime_test n = [n, first_index]
    where 
        checkBase base = (hcf n base == 1) && (fermatPseudoPrime n base)
        tests = map checkBase [2..ceiling $ sqrt $ fromInteger n]
        first_index = case elemIndex False tests of Just i -> toInteger (i+1); Nothing -> -1

carmichaelNumber = absolutePseudoPrimeTest fermatPseudoPrime
temp = absolutePseudoPrimeTest2 fermatPseudoPrime

-- Returns pseudo_primes_base_2 and absolute_pseudo_primes
tester::(Integer -> Integer -> Bool) -> Integer -> [[Integer]]
tester pseudoprime_base_test upper_bound = 
    [getPseudoPrimes pseudoprime_base_test [2] inrange, filter is_absolute_pseudo_prime inrange]
        where 
            is_absolute_pseudo_prime = absolutePseudoPrimeTest pseudoprime_base_test
            inrange = [x | x <- [3..upper_bound], x `mod` 2 == 1]


upper_bound = 10000
q3 = tester fermatPseudoPrime upper_bound
q3_bases = map (basesToCheck fermatPseudoPrime) [x | x <- [2..upper_bound]]
q4 = tester eulerPseudoPrime upper_bound
q5 = tester strongBaseTest upper_bound


getPrimesLeq:: Integer -> [Integer] -- returns all primes 2,3,... less than n
getPrimesLeq 1 = []
getPrimesLeq 2 = [2]
getPrimesLeq n = let plist = getPrimesLeq (n-1); f x numlist = any (\y -> x `mod` y == 0) (filter (< (ceiling $ sqrt $ fromInteger n)) numlist) in
    if f n plist then plist else plist ++ [n]

{-
getPrimesLeq2:: Integer -> [Integer] -- returns all primes 2,3,... less than n
getPrimesLeq2 n = let plist = [2..n] in delete a 
getPrimesLeq2 1 = []
getPrimesLeq2 2 = [2]
getPrimesLeq2 n = let plist = getPrimesLeq (n-1); f x numlist = any (\y -> x `mod` y == 0) (filter (< (ceiling $ sqrt $ fromInteger n)) numlist) in
    if f n plist then plist else plist ++ [n]

removePmults:: [Integer] -> Integer -> Integer -> [Integer]
removePmults num_list length_num_list p = map (\i -> ) [p,2*p.. p * (floor n/p)]    
-}

-- #################
-- Q3
eulerStep::Integer -> Integer -> [Integer] -- rearranges s.t. a > b > 0 and then returns [b, a - p*b = r] with b > r >= 0 
eulerStep a b = 
    let x=minimum [a,b]; y=maximum [a,b]; p = floor (fromInteger y/ fromInteger x)
    in [x, y - p*x]

hcf::Integer -> Integer -> Integer -- highest common factor
hcf a 0 = a
hcf 0 b = b
hcf a 1 = 1
hcf 1 b = 1
hcf a b = let [x,y] = eulerStep a b in hcf x y


-- speed up by e.g. if N-1 is even then a^(N-1) = (-a)^(N-1), o/w = -(-a)^(N-1). (So actually if N is even we're guaranteed it isn't a carmichael number unless N=2) either way only need to check half the bases This doesn't seem to work for small bases



-- To deterine if N is prime / composite using fermatPseudoPrime testing bases, given that N isn't a carmichael number:
-- half bases don't have a^(N-1) % N == 1. Further N composite iff it has a factor below sqrt(N), for which  a^(N-1) % N != 1. Therefore we can check up to sqrt(N) bases to see if all 1 or not.

jacobi::Integer -> Integer -> Integer -- 0, 1 or -1. m should be odd. is product of legendre symbols m on p_i
jacobi 1 n = 1
jacobi 0 n = 0
jacobi (-1) n = if n `mod` 4 == 1 then 1 else -1
jacobi 2 n = let t = n `mod` 8 in if t `elem` [1,7] then 1 else -1
jacobi m n  
    | m `mod` 2 == 0    = (jacobi 2 n) * (jacobi (m `div` 2) n)
    | m < 0             = (jacobi (-1) n) * (jacobi (-m) n)
    | m >= n            = jacobi (m `mod` n) n
    | m < n             = (if m `mod` 4 == 1 || n `mod` 4 == 1  then 1 else -1) * jacobi n m

eulerPseudoPrime::Integer -> Integer -> Bool
eulerPseudoPrime n a = (jacobi a n) `mod` n == a^((n-1) `div` 2) `mod` n

absoluteEulerPseudoPrime = absolutePseudoPrimeTest eulerPseudoPrime


q4_euler_pseudo_primes_base_2 = let passPseudoPrimeTest = filter (\n -> eulerPseudoPrime n 2) [1..10^6] in filter (not . isPrimeTrialDivide) passPseudoPrimeTest
q4_absolute_euler_pseudoprimes = filter (absoluteEulerPseudoPrime) [1..10^6] -- supposed to be an empty list

-- again only need to check up to sqrt bases if using eulerPseudoPrime as a test of primality / composity (Why is this question asked?)

stripEven::Integer -> (Integer, Integer)
stripEven n
    | n == 0             = (0, 1)
    | n `mod` 2 /= 0    = (0, n)
    | n `mod` 2 == 0    = let(x,y) = stripEven (n `div` 2) in (x+1, y)


strongBaseTest::Integer -> Integer -> Bool
strongBaseTest n a = a^s `mod` n == 1 || any (\x -> x `mod` n == (-1) `mod` n) [a^(s*2^k) | k <- [0..(r-1)]]
    where (r, s) = stripEven (n-1) -- n-1 = s * 2^r

absoluteStrongBaseTest = absolutePseudoPrimeTest strongBaseTest


q6::Integer -> [Integer] -> [[Integer]] -- We note that passing strong base test => passing euler pseudo prime test => passing fermat pseudo prime test. ACTUALLY 3277 base 2? I think I've fixed this now.
q6 k bases = 
    [
        getPseudoPrimes fermatPseudoPrime bases test_list,
        getPseudoPrimes eulerPseudoPrime bases test_list,
        getPseudoPrimes strongBaseTest bases test_list
    ]
    where test_list = [10^k..10^k + 10^5]


q6_base2 = [q6 k [2] | k <-[5..9]]
q6_base3 = [q6 k [3] | k <-[5..9]]
q6_base_2_3 = [q6 k [2,3] | k <-[5..9]]

q6_primes = [filter isPrimeTrialDivide [10^k..10^k + 10^5] | k <-[5..9]]

q6_base2_sets = map (map Data.Set.fromList) q6_base2
-- q6_base2_subsets = let  q6_base2_pairs = map (\x -> zip x (tail x)) q6_base2_sets in 
--                         map (\pair -> Data.Set.isSubsetOf (pair!!0) (pair!!1)) q6_base2_pairs

primes = [2,3,5,7,11,13]
asdf = [2..14]
asdf2 = Data.List.Ordered.minus asdf primes


main = do
    let fn = "non_absoloute_pseudoprime_bases_to_check.txt"

    putStrLn "reading primes list"
    let fn2 = "primes_below_million.txt"
    let fn3 = "pseudoprime_bases_to_check.txt"

    primes_list_str <- readFile fn2
    let primes_list = map (\x -> read x :: Integer) (splitOn ", " primes_list_str)
    
    pseudoprimes_list_str <- readfile fn3
    let pseudoprimes_list = map (\x -> read x :: Integer) (splitOn ", " pseudoprimes_list_str)


    writeFile fn ""

    let write name contents = appendFile fn (name ++ (show contents) ++ "\n\n") 
    --appendFile fn ("q1: " ++ show q1_answers)
    let stringify x = case x of NumPrimeBase n _ bases -> (show n) ++ " && " ++ show bases
    --write "q1: " q1_answers
    --write "q2: " (intercalate "\n" $ map stringify (q2_answers!!0))
    --write "q3: " q3
    --write "q4: " q4
    --write "q5: " q5

    let ub = 1000000
    let noprimes = Data.List.Ordered.minus [2..ub] primes_list --remove primes
    -- apply psuedoprime tests
    let q3_bases_better = map (basesToCheck fermatPseudoPrime) (Data.List.Ordered.minus [2..ub] primes_list)

    -- filter for interesting ones
    let q3_bases_better' = filter (\x -> case x of [_,1] -> False; _ -> True) q3_bases_better

    write "" q3_bases_better'