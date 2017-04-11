
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


sumaAlt :: (Num a) => [a] -> a
sumaAlt = foldr (\x recu -> x - recu) 0

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
-- mal
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


-- Ejercicio 15

sumaMat ::[[Int]] -> [[Int]] -> [[Int]]
sumaMat (xs:xss) (ys:yss) = foldr f (const []) (ys:yss) (xs:xss)
  where f ys recu = \(xs:xss) -> (zipWith (+) xs ys) : (recu xss)

t :: [a] -> [[a]] -> [[a]]
t xs xss = foldr f (const []) xs xss
  where f x recu = \yss -> if null xss then ([x] : (recu [])) else ([x] ++ (head yss)) : (recu (tail yss))

trasponer :: [[a]] -> [[a]]
trasponer = foldr t []

zipWithList :: (a -> b -> b) -> b -> [[a]] -> [b]
zipWithList f b xss = foldr g [] (trasponer xss)
  where g xs recu = (foldr f b xs) : recu


-- Ejercicio 16

generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs
    | stop xs = init xs
    | otherwise = generateFrom stop next (xs ++ [next xs])

generateBase :: ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop b next =  generateFrom stop (next . last) [b]

factoriales :: Int -> [Int]
factoriales n = generate (\l -> length l > n) (\l -> factorial ((length l) + 1))
  where factorial n = foldl (*) 1 [1..n]

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = generateBase (\l -> length l > n) x f


generateFrom2 :: (a -> Bool) -> (a -> a) -> a -> [a]
generateFrom2 stop next x = takeWhile f (iterate next x)
  where f y = not (stop y)


-- Ejercicio 17

foldNat :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
foldNat f b 0 = b
foldNat f b n = f n (foldNat f b (n-1))

potencia :: Integer -> Integer -> Integer
potencia x = foldNat (const (*x)) 1


-- Ejercicio 18 -- PREGUNTAR FOLD
data Polinomio a = X
  | Cte a
  | Suma (Polinomio a) (Polinomio a)
  | Prod (Polinomio a) (Polinomio a)

foldPoli :: (Num b) => (a -> b) -> b -> Polinomio a -> b
foldPoli fCte b poli = case poli of X -> b
                                    Cte a -> fCte a
                                    Suma p1 p2 -> (foldPoli fCte b p1) + (foldPoli fCte b p2)
                                    Prod p1 p2 -> (foldPoli fCte b p1) * (foldPoli fCte b p2)

evaluar :: (Num a) => a -> Polinomio a -> a
evaluar x = foldPoli id x

-- 2x² + 5x + 10
-- (Suma (Suma (Prod (Prod X X) (Cte 2)) (Prod X (Cte 5))) (Cte 10))

-- Ejercicio 19

type Conj a = (a -> Bool)

vacio :: Conj a
vacio = const False

agregar :: (Eq a) => a -> Conj a -> Conj a
agregar n conj = \x -> x == n || conj x

interseccion :: Conj a -> Conj a -> Conj a
interseccion c1 c2 = \x -> (&&) (c1 x) (c2 x)

union :: Conj a -> Conj a -> Conj a
union c1 c2 = \x -> (||) (c1 x) (c2 x)

-- El conjunto de funciones de un parametro que devuelven Bool
-- conjInfFunciones :: Conj (a->Bool)
-- conjInfFunciones conj = \f -> \x -> (f x) == True

primeraAparicion :: a -> [Conj a] -> Int
primeraAparicion e (x:xs) = if (x e) then 0 else 1 + (primeraAparicion e xs)

conjunto1 = agregar 'a' (agregar 'b' (agregar 'c' (agregar 'a' vacio)))
conjunto2 = agregar 'a' (agregar 'd' (agregar 'a' vacio))
conjunto3 = agregar 'b' (agregar 'd' (agregar 'e' vacio))
listaInfinita = conjunto1:conjunto2:conjunto3:listaInfinita


-- Ejercicio 20

data AHD tInterno tHoja = Hoja tHoja
                          | Rama tInterno (AHD tInterno tHoja)
                          | Bin (AHD tInterno tHoja) tInterno (AHD tInterno tHoja)

foldAHD :: (tHoja -> b) -> (tInterno -> b -> b) -> (b -> tInterno -> b -> b) -> AHD tInterno tHoja -> b
foldAHD fHoja fInterno f arbol = case arbol of 
                                  Hoja tHoja -> fHoja tHoja
                                  Rama tInterno subArbol -> fInterno tInterno (foldAHD fHoja fInterno f subArbol)
                                  Bin a1 r a2 -> f (foldAHD fHoja fInterno f a1) r (foldAHD fHoja fInterno f a2)

mapAHD :: (a -> b) -> (c -> d) -> AHD a c -> AHD b d
mapAHD fInterno fHoja = foldAHD f g h
                          where f = (\h -> Hoja (fHoja h))
                                g = (\r a1 -> Rama (fInterno r) a1)
                                h = (\a1 r a2 -> Bin a1 (fInterno r) a2)


hojas :: AHD tInterno tHoja -> [tHoja]
hojas arbol = foldAHD (\h -> [h]) (const id) (\l1 r l2 -> l1 ++ l2) arbol

tieneRepetidas :: (Eq tHoja) => AHD tInterno tHoja -> Bool
tieneRepetidas arbol = foldr (\h rec -> \hs -> (elem h hs) || (rec (tail hs))) (const False) (hojas arbol) (tail (hojas arbol))

analizar :: (Eq tHoja) => AHD tInterno tHoja -> Either (tHoja -> Int) [tInterno]
analizar arbol = if tieneRepetidas arbol then Left (f arbol) else Right (dfs arbol)
                      where f arbol = \ x -> foldr (\h rec -> if h == x then 1 + rec else rec) 0 (hojas arbol)
                            dfs = foldAHD (const []) (\r l -> r : l) (\l1 r l2 -> (r : l1) ++ l2)


leftResult :: Either a b -> a
leftResult (Left a) = a

rightResult :: Either a b -> b
rightResult (Right b) = b


-- Ejercicio 21

data AB a = Nil | ABin (AB a) a (AB a)

foldAB :: (b -> a -> b -> b) -> b -> AB a -> b
foldAB f b Nil = b
foldAB f b (ABin a1 r a2) = f (foldAB f b a1) r (foldAB f b a2)

esNil :: AB a -> Bool
esNil arbol = case arbol of Nil -> True
                            ABin a1 r a2 -> False

altura :: AB a -> Int
altura = foldAB (\rec1 raiz rec2 -> (max rec1 rec2) + 1) 0

ramas :: AB a -> [[a]] -- Si, muy cabeza
ramas = foldAB (\rec1 raiz rec2 -> if null rec1 && null rec2 then [[raiz]] 
                                    else if null rec1 then [raiz:xs | xs <- rec2]
                                      else if null rec2 then [raiz:xs | xs <- rec1]
                                        else [raiz:xs | xs <- rec1] ++ [raiz:xs | xs <- rec2]) []

cantNodos :: AB a -> Int
cantNodos = foldAB (\rec1 raiz rec2 -> rec1 + rec2 + 1) 0 

cantHojas :: AB a -> Int
cantHojas = foldAB (\rec1 raiz rec2 -> if rec1 + rec2 == 0 then 1 else rec1 + rec2) 0

espejo :: AB a -> AB a
espejo = foldAB (\a1 raiz a2 -> ABin a2 raiz a1) Nil

-- ABin (ABin (Nil) 2 (ABin (Nil) 3 (Nil))) 1 (ABin (ABin Nil 6 Nil) 4 (ABin Nil 5 Nil))


-- Ejercicio 22

data RoseTree a = RoseTree a [RoseTree a]

foldRoseT :: (a -> [b] -> b) -> (a -> b) -> RoseTree a -> b 
foldRoseT f b (RoseTree a []) = b a
foldRoseT f b (RoseTree a rs) = f a (map (foldRoseT f b) rs)

hojasR :: RoseTree a -> [a]
hojasR = foldRoseT (\_ l -> concat l) (flip (:) [])

distanciasR :: RoseTree a -> [Int]
distanciasR = foldRoseT (\_ l -> map (+1) (concat l)) (const [0]) 

alturaR :: RoseTree a -> Int
alturaR arbol = foldr1 max (distanciasR arbol) + 1

-- RoseTree 1 [(RoseTree 2 [])]
-- RoseTree 1 [(RoseTree 2 [RoseTree 3 []]), (RoseTree 4 []), (RoseTree 5 [(RoseTree 6 [])])]






main = print 1