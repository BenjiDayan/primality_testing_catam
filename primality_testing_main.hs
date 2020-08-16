import PrimalityTesting
import Data.List.Split
import System.Random
import Data.List
import qualified Data.Set
import qualified Data.List.Ordered

absoluteFermatPseudoPrime = absolutePseudoPrimeTest fermatPseudoPrime


q1_getPrimes::Integer -> Integer -> [Integer] -- Returns list of primes in [lower bound, upper bound]
q1_getPrimes lb ub = filter isPrimeTrialDivide [lb..ub]

q1_answers = [q1_getPrimes 1900 1999, q1_getPrimes 1294268500 1294268700]


data NumPrimeBase = NumPrimeBase Integer Bool [Integer]
    deriving Show

q2_answers = [
    filter is_PP_some_base [NumPrimeBase n (isPrimeTrialDivide n)  (is_PP_bases n) | n <- range]
    | range <- [[1900..1999],[1294268500..1294268700]]
    ]
    where   is_PP_some_base x = case x of NumPrimeBase _ False [1] -> False; _ -> True
            is_PP_bases n = filter (fermatPseudoPrime n) [1..13]
    


upper_bound = 1000000
primes = filter isPrimeTrialDivide [2..upper_bound]
num_list = removePrimes [x | x <- [2..upper_bound], x `mod` 2 == 1] primes
q3_bases_checked = numBasesCheckedNice num_list fermatPseudoPrime
q4_bases_checked = numBasesCheckedNice num_list eulerPseudoPrime
q5_bases_checked = numBasesCheckedNice num_list strongBaseTest

q3_bases_checked' = filter (\x -> not . absoluteFermatPseudoPrime $ x!!0) q3_bases_checked



removePrimes num_list primes_list = Data.List.Ordered.minus num_list primes_list

-- removes numbers which fail pseudo_prime_test on first base (2)
numBasesCheckedNice num_list pseudo_prime_test = 
    let bases_checked = map (basesToCheck pseudo_prime_test) num_list
    in filter (\x -> case x of [_,1] -> False; _ -> True) bases_checked


q6::Integer -> [Integer] -> [[Integer]] 
q6 k bases = 
    [
        getPseudoPrimes fermatPseudoPrime bases test_list,
        getPseudoPrimes eulerPseudoPrime bases test_list,
        getPseudoPrimes strongBaseTest bases test_list
    ]
    where test_list = [x | x <- [10^k..10^k + 10^5], x `mod` 2 == 1]


q6_base2 = [q6 k [2] | k <-[5..9]]
q6_base3 = [q6 k [3] | k <-[5..9]]
q6_base_2_3 = [q6 k [2,3] | k <-[5..9]]

-- alternative arrangement
z1 = [getPseudoPrimes fermatPseudoPrime [2] test_list | test_list <-[[10^k..10^k+10^5] | k <-[5..9]]]
z2 = [getPseudoPrimes fermatPseudoPrime [3] test_list | test_list <-[[10^k..10^k+10^5] | k <-[5..9]]]
z3 = [getPseudoPrimes fermatPseudoPrime [2,3] test_list | test_list <-[[10^k..10^k+10^5] | k <-[5..9]]]


q6_primes = [filter isPrimeTrialDivide [10^k..10^k + 10^5] | k <-[5..9]]




-- #### Q7 ####
newRand = randomIO :: IO Int
randomListDouble :: Int -> [Double]
randomListDouble seed = randoms (mkStdGen seed) :: [Double]
randomListInt :: Int -> [Int]
randomListInt seed = randoms (mkStdGen seed) :: [Int]

-- -5733685665416936678 is an example random number, about 10^19
q7_getRandIntegers:: Int -> Integer -> [Integer] 
q7_getRandIntegers random_seed upper_bound = [toInteger (x `mod` ub) | x <- random_int_list]
    where random_int_list = randomListInt random_seed; ub = fromInteger upper_bound

-- Returns True/False if n is prime (though False if n is any of [2,3,5,7,11])
q7_isPrime n = if not $ q7_isPrimeProb n then False else isPrimeTrialDivide n
q7_isPrimeProb n = if any (not . strongBaseTest n) [2,3,5,7,11] then False else True

-- doing random ranges in [0..10^k] for k = [5..12]. experiment to plot time of q8_isPrime
random_seed = 8551880565528615866
random_seeds = randomListInt random_seed

 -- s.t. each length operation takes roughly the same amount of time. Capped to save memory
ubToLength n = minimum [floor $ (2*10^9)/(sqrt $ fromInteger n), 18000]
upper_bounds = [10^k | k <- [7..13]]
--[10^5, 3*10^5, 10^6, ...]
upper_bounds2 = concat $ transpose [upper_bounds, [3*ub | ub <- upper_bounds]] 
num_took = map ubToLength upper_bounds2
random_ints = [take (ubToLength ub) $ q7_getRandIntegers seed ub |
    (seed, ub) <- zip random_seeds upper_bounds2] 
ranges_primes = [filter q7_isPrime num_list | num_list <- random_ints]

-- #### Q8 ####
k = 20
q8_getRandIntegers random_seed k = [toInteger $ f x | x <- random_int_list]
    where random_int_list = randomListInt random_seed; f x = 2^(k-1) + (x `mod` 2^k)
random_kbits = q8_getRandIntegers 214349420844060056 k
random_kbits_bases = numBasesCheckedNice random_kbits strongBaseTest 
random_kbits_non_prime = filter (\x -> case x of [_,-1] -> False; _ -> True) random_kbits_bases


main = do
    let fn = "non_absoloute_pseudoprime_bases_to_check.txt"
    let fn2 = "primes_below_million.txt"
    let fn3 = "pseudoprime_bases_to_check.txt"

    let write name contents = appendFile fn (name ++ (show contents) ++ "\n\n") 

    
    pseudoprimes_list_str <- readFile fn3
    let pseudoprimes_list = map (\x -> read x :: Integer) (splitOn ", " pseudoprimes_list_str)


    -- bases to check for Q3
    primes_list_str <- readFile fn2
    let primes_list = map (\x -> read x :: Integer) (splitOn ", " primes_list_str)
    let ub = 10000
    let noprimes = Data.List.Ordered.minus [2..ub] primes_list --remove primes
    -- apply psuedoprime tests
    let q3_bases_better = map (basesToCheck fermatPseudoPrime) noprimes
    -- filter for interesting ones
    let q3_bases_better' = filter (\x -> case x of [_,1] -> False; _ -> True) q3_bases_better
    write "" q3_bases_better'

    putStrLn "HI IS MAIN"

