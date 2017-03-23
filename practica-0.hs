
-- Ejercicio 2
valorAbsoluto :: Float -> Float
valorAbsoluto x
    | x < 0 = -x
    | otherwise = x

bisiesto :: Int -> Bool
bisiesto n
    | mod n 400 == 0 = True
    | mod n 4 == 0 && mod n 100 /= 0 = True
    | otherwise = False

factorial :: Int -> Int
factorial n
    | n == 1 = 1
    | otherwise = n * factorial (n - 1)

    -- Auxiliares

divisores :: Int -> [Int] -> [Int]
divisores n [] = []
divisores n (x:xs)
    | mod n x == 0 =  x : divisores n xs
    | otherwise = divisores n xs

esPrimo :: Int -> Bool
esPrimo 1 = True
esPrimo n
    | length (divisores n [2..(n-1)]) == 0 = True
    | otherwise = False

divisoresPrimos :: Int -> [Int] -> [Int]
divisoresPrimos n [] = []
divisoresPrimos n (x:xs)
    | esPrimo x = x : divisoresPrimos n xs
    | otherwise = divisoresPrimos n xs

    -- 

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos 1 = 1
cantDivisoresPrimos n = length (divisoresPrimos n (divisores n [2..n]) )
-- 

-- Ejercicio 3

    -- data Maybe a = Nothing | Just a
    -- data Either a b = Left a | Right b

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (x ** (-1))

aEntero :: Either Int Bool -> Int
aEntero (Right b)
    | b = 1
    | not b = 0

aEntero (Left n) = n

-- 

-- Ejercicio 4

limpiar :: String -> String -> String
limpiar s [] = []
limpiar xs (y:ys)
    | not (elem y xs) = y : limpiar xs ys
    | otherwise = limpiar xs ys

    -- Auxiliares

promedio :: [Float] -> Float
promedio xs = sum(xs) / fromIntegral(length xs)

    -- 

difPromedio :: [Float] -> [Float]
difPromedio xs = [x - promedio(xs) | x <- xs]


todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales (x:[]) = True
todosIguales (x:xs)
    | not (elem x xs) = False
    | otherwise = todosIguales xs

-- 

-- Ejercicio 5

data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB a = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin a1 r a2)
    | (vacioAB a1) && (vacioAB a2) = Bin Nil (not r) Nil
    | (vacioAB a1) = Bin Nil (not r) (negacionAB a2)
    | (vacioAB a2) = Bin (negacionAB a1) (not r) Nil
    | otherwise = Bin (negacionAB a1) (not r) (negacionAB a2)

productoAB :: AB Int -> Int
productoAB (Bin a1 r a2)
    | (vacioAB a1) && (vacioAB a2) = r
    | (vacioAB a1) = r * (productoAB a2)
    | (vacioAB a2) = r * (productoAB a1)
    | otherwise = r * (productoAB a1) * (productoAB a2)
-- 




main = print (valorAbsoluto 2)