module NavesEspaciales (Componente(Contenedor, Motor, Escudo, Cañón), NaveEspacial(Módulo, Base), Dirección(Babor, Estribor), TipoPeligro(Pequeño, Grande, Torpedo), Peligro, foldNave, capacidad, poderDeAtaque, puedeVolar, mismoPotencial, mayorCapacidad, transformar, impactar, maniobrar, pruebaDeFuego, componentesPorNivel, dimensiones) where

data Componente = Contenedor | Motor | Escudo | Cañón deriving (Eq, Show)

data NaveEspacial = Módulo Componente NaveEspacial NaveEspacial | Base Componente deriving Eq

data Dirección = Babor | Estribor deriving Eq

data TipoPeligro = Pequeño | Grande | Torpedo deriving Eq

type Peligro = (Dirección, Int, TipoPeligro)

instance Show NaveEspacial where
  show = ("\n" ++) . (padNave 0 0 False)
  
padNave nivel acum doPad (Base c) = (if doPad then pad (4*nivel + acum) else "") ++ show c
padNave nivel acum doPad (Módulo x i d) = (if doPad then pad (4*nivel + acum) else "") ++ show x ++ 
					  pad 4 ++ padNave (nivel+1) (acum+l) False i ++ "\n" ++
					  padNave (nivel+1) (acum+l) True d where l = length $ show x

pad :: Int -> String
pad i = replicate i ' '

--Ejercicio 1
foldNave :: (Componente -> b) -> (Componente -> b -> b -> b) -> NaveEspacial -> b
foldNave fBase fModulo (Base comp) = fBase comp
foldNave fBase fModulo (Módulo comp subNave1 subNave2) = fModulo comp (foldNave fBase fModulo subNave1) (foldNave fBase fModulo subNave2)

--Ejercicio 2

recorrerNaveDevolviendo :: Componente -> NaveEspacial -> Int
recorrerNaveDevolviendo cmpnt = foldNave (\comp -> if comp == cmpnt then 1 else 0) fModulo
 				 where fModulo = (\comp recursionIzq recursionDer -> if (comp == cmpnt) then 1 + recursionIzq + recursionDer  else recursionIzq + recursionDer)

capacidad :: NaveEspacial -> Int
capacidad = recorrerNaveDevolviendo Contenedor

poderDeAtaque :: NaveEspacial -> Int
poderDeAtaque = recorrerNaveDevolviendo Cañón

poderDeDefensa :: NaveEspacial -> Int
poderDeDefensa = recorrerNaveDevolviendo Escudo

aceleracion :: NaveEspacial -> Int
aceleracion = recorrerNaveDevolviendo Motor

puedeVolar :: NaveEspacial -> Bool
puedeVolar = (0 <).(recorrerNaveDevolviendo Motor)

mismoPotencial :: NaveEspacial -> NaveEspacial -> Bool
mismoPotencial nave1 nave2 = capacidad nave1 == capacidad nave2 && poderDeAtaque nave1 == poderDeAtaque nave2 && aceleracion nave1 == aceleracion nave2 && poderDeDefensa nave1 == poderDeDefensa nave2

-- Ejercicio 3
mayorCapacidad :: [NaveEspacial] -> NaveEspacial
mayorCapacidad = undefined

-- Ejercicio 4
transformar :: (Componente -> Componente) -> NaveEspacial -> NaveEspacial
transformar = undefined

-- Ejercicio 5
impactar :: Peligro -> NaveEspacial -> NaveEspacial
impactar = undefined

-- Ejercicio 6
maniobrar :: NaveEspacial -> [Peligro] -> NaveEspacial
maniobrar = undefined

-- Ejercicio 7
pruebaDeFuego :: [Peligro] -> [NaveEspacial] -> [NaveEspacial]
pruebaDeFuego = undefined

-- Ejercicio 8
componentesPorNivel :: NaveEspacial -> Int -> Int
componentesPorNivel = undefined

dimensiones :: NaveEspacial -> (Int, Int)
dimensiones = undefined
