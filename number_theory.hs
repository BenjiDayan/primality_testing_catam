# position :: Integer -> [Integer] -> Integer
# position y xs = head [i | (i, z) <- zip [1..] xs, z==y]

get_lcm :: [Integer] -> Integer
get_lcm [x] = x  
get_lcm (x:[y]) = head [y*i | i <- [1..], y*i `mod` x == 0] 
get_lcm (x:xs) = get_lcm [x, get_lcm xs]

generate_cycles :: Integer -> Integer -> [Integer]
generate_cycles a p = [a^i `mod` p | i <- [1..p-1]]

get_order :: Integer -> Integer -> Integer
get_order a p = generate_cycles

get_lcm_order :: Integer -> Integer
get_lcm_order p = get_lcm [list
