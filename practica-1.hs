
-- Ejercicio 2

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f = \x y -> f (x,y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f = \(x,y) -> f x y

-- 
-- Ejercicio 4
-- [1..] no termina nunca

-- Chequar esto
pitagoricas :: Int -> [(Int, Int, Int)]
pitagoricas n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]

-- 
-- Ejercicio 5

-- esPrimo :: Int -> Bool
-- esPrimo = \n -> (.) length divisores n == 0
-- 	where divisores = \n -> (filter (\x -> mod n x == 0) [2..f n])
-- 		where f = \n -> floor (sqrt n)


-- Preguntar
-- 
-- Ejercicio 6

partir :: [a] -> [([a], [a])]
partir xs = [(take x xs, take (length xs - x) xs) | x <- [0..length xs]]

-- 
-- Ejercicio 7

listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listasQueSuman n = [x : ys | x <- [1..n], y <- [0..n-1], ys <- listasQueSuman y, x + sum (filter (\k -> k <= x) ys) == n]
-- 
-- Ejercicio 8

-- [ys | x <- [1..], ys <- [[1..x]]]

-- 



main = print 1