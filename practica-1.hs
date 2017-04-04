
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
pitagoricas n = take n [(a,b,c) | a <- [1..], b <- [1..], c <- [1..], a^2 + b^2 == c^2]

-- 
-- Ejercicio 5

-- esPrimo :: Int -> Bool
-- esPrimo = \n -> (.) length divisores n == 0
--  where divisores = \n -> (filter (\x -> mod n x == 0) [2..f n])
--      where f = \n -> floor (sqrt n)

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
-- Ejercicio 9

type DivideConquer a b = (a -> Bool)    -- determina si es o no el caso trivial
                        -> (a -> b)     -- resuelve el caso trivial
                        -> (a -> [a])   -- parte el problema en sub-problemas
                        -> ([b] -> b)   -- combina resultados
                        -> a            -- input
                        -> b            -- resultado

dc :: DivideConquer a b
dc trivial solve split combine x
    | trivial x = solve x
    | otherwise = combine $ map (dc trivial solve split combine) $ split x

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort = dc trivial solve split combine
    where trivial (x:xs) = xs == []
          solve = id
          split xs = [sublista xs [0..div (length xs) 2 - 1], sublista xs [div (length xs) 2..length xs - 1]]
            where sublista xs = map (\y -> xs!!y)
          combine (xs:ys:[]) = merge xs ys

mapDC :: (a -> b) -> [a] -> [b]
mapDC f = dc trivial solve split combine
    where trivial [] = True
          trivial (x:[]) = True
          trivial (x:xs) = False
          solve [] = []
          solve (x:[]) = [f x]
          split (x:xs) = [[x], xs]
          combine ((x:[]):xs:[]) = x : xs

filterDC :: (a -> Bool) -> [a] -> [a]
filterDC p = dc trivial solve split combine
    where trivial [] = True
          trivial (x:[]) = True
          trivial (x:xs) = False
          solve [] = []
          solve (x:[]) = if (p x) then x:[] else []
          split (x:xs) = [[x], xs]
          combine (xs:ys:[]) = xs ++ ys

-- 
-- Ejercicio 10

sum2 :: (Num a) => [a] -> a
sum2 = foldr (+) 0

elem2 :: (Eq a) => a -> [a] -> Bool
elem2 x = foldr (\e -> (||) ((==) e x)) False 

concatenar :: [a] -> [a] -> [a]
concatenar xs ys = foldr (:) ys xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p = foldr (\x ->(++) (f x)) []
    where f = \x -> if (p x) then x:[] else []

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr (\x -> (:) (f x)) []



-- Preguntar
-- sumaAlt :: (Num a) => [a] -> a
-- sumaAlt xs= foldr (f (length xs)) 0 xs
--     where f x = if (even x) then flip (-) else (+)


-- Ejercicio 11

partes :: [a] -> [[a]]
partes = foldr (\x xss-> [x : ys | ys <- xss] ++ xss) [[]]

prefijos :: [a] -> [[a]]
prefijos = foldr (\x xss -> [] : [x : ys | ys <- xss]) [[]]

-- prefijos [] = [[]]
-- prefijos (x:xs) = [] : [x : ys | ys <- prefijos xs]

sublista :: [a] -> [[a]]
sublista = foldr (\x xss -> xss ++ [take n (x:(last xss))| n <- [1..length (last xss) + 1]]) [[]]

-- Ejercicio 12

sacarUna :: (Eq a) => a -> [a] ->[a]

sacarUna _ [] = []
sacarUna x xs
    | elem x xs = fst (break (x==) xs) ++ tail (snd (break (x==) xs))
    | otherwise = xs

permutaciones :: (Eq a) => [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = [x : ys | x <- xs, ys <- permutaciones (sacarUna x xs)]

-- Ejercicio 13

genLista :: a -> (a -> a) -> Integer -> [a]
genLista _ _ 0 = []
genLista e f n = e : (genLista (f e) f (n - 1))


desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta x y = genLista x succ ((y - x) + 1)

-- Ejercicio 14

mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (\(x,y) -> f x y)

armarPares :: [a] -> [b] -> [(a,b)]
armarPares _ [] = []
armarPares [] _ = []
armarPares (x:xs) (y:ys) = (x,y) : armarPares xs ys

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = (mapPares f) $ armarPares xs ys

main = print 1