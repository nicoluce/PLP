module Main where
import NavesEspaciales
import Test.HUnit
import Data.List



--Naves para pruebas:
contenedorSolo = Base Contenedor
nave1 = Base Motor
nave2 = Módulo Cañón (Base Escudo) (Base Motor)
nave3 = Módulo Motor (Base Escudo) (Base Cañón)
nave4 = Módulo Contenedor nave2 nave3
nave5 = Módulo Contenedor nave3 nave2
nave6 = Módulo Contenedor nave4 nave1
nave7 = Módulo Contenedor nave1 nave5
nave8 = Módulo Contenedor nave1 nave6
nave9 = Módulo Escudo 
		(Módulo Escudo (Módulo Escudo (Base Escudo) (Base Cañón)) (Módulo Motor (Base Contenedor) (Base Motor))) 
		(Módulo Escudo (Módulo Contenedor (Base Motor) (Base Contenedor)) (Módulo Escudo (Base Cañón) (Base Escudo)))
--nave10 = nave9 con reemplazos (Escudo por Cañón, Cañón por Contenedor, Contenedor por Motor, Motor por Escudo)
nave10 = Módulo Cañón 
    (Módulo Cañón (Módulo Cañón (Base Cañón) (Base Contenedor)) (Módulo Escudo (Base Motor) (Base Escudo))) 
    (Módulo Cañón (Módulo Motor (Base Escudo) (Base Motor)) (Módulo Cañón (Base Contenedor) (Base Cañón)))

nave11 = Módulo Motor nave1 nave1
naveRestante = Módulo Contenedor (Base Contenedor) (Base Contenedor)
naveChatarra = Módulo Contenedor (Base Contenedor) (Base Contenedor)
naveIgualNoAndaba = Módulo Contenedor (Módulo Contenedor (Base Contenedor) (Base Contenedor)) (Módulo Contenedor (Base Contenedor) (Base Contenedor))


--naves que figuran en la consigna del tp
naveEj11 = Módulo Contenedor (Módulo Escudo (Base Cañón) (Base Motor)) (Base Motor)
naveEj12 = Módulo Contenedor (Base Contenedor) (Base Motor)
naveEj13 = Módulo Contenedor (Módulo Escudo (Base Motor) (Base Motor)) (Base Motor)


soloUnMotor = Base Motor
puroContenedor = Módulo Contenedor (Base Contenedor) (Base Contenedor)
tresCañones = Módulo Cañón (Base Cañón) (Base Cañón)

contenedorYCañon = Módulo Contenedor (Base Cañón) (Base Contenedor)
otroCañon = Módulo Contenedor (Base Contenedor) (Base Cañón)

escudoSinCañon = Módulo Escudo (Base Contenedor) (Base Contenedor)

protegido = Módulo Escudo (Base Contenedor) (Base Cañón)
protegidoNivel1Estribor = Módulo Contenedor soloUnMotor protegido

superProtegido = Módulo Motor protegido protegido

desbalanceado = Módulo Escudo (Base Contenedor) protegido

altaNave = Módulo Contenedor (Módulo Escudo (Módulo Escudo (Base Cañón) (Base Cañón)) (Base Motor)) (Base Motor)
naveNoMeDan = Módulo Motor (Módulo Escudo (Base Cañón) (Base Cañón)) (Módulo Escudo (Base Cañón) (Base Cañón))


--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8
  ]

testsEj2 = test [
  0 ~=? capacidad soloUnMotor,
  3 ~=? capacidad puroContenedor,
  0 ~=? capacidad tresCañones,
  2 ~=? capacidad contenedorYCañon,
  2 ~=? capacidad otroCañon,
  2 ~=? capacidad escudoSinCañon,
  1 ~=? capacidad protegido,
  2 ~=? capacidad protegidoNivel1Estribor,
  2 ~=? capacidad superProtegido,
  2 ~=? capacidad desbalanceado,

  0 ~=? poderDeAtaque soloUnMotor,
  0 ~=? poderDeAtaque puroContenedor,
  3 ~=? poderDeAtaque tresCañones,
  1 ~=? poderDeAtaque contenedorYCañon,
  1 ~=? poderDeAtaque otroCañon,
  0 ~=? poderDeAtaque escudoSinCañon,
  1 ~=? poderDeAtaque protegido,
  1 ~=? poderDeAtaque protegidoNivel1Estribor,
  2 ~=? poderDeAtaque superProtegido,
  1 ~=? poderDeAtaque desbalanceado,

  True ~=? puedeVolar soloUnMotor,
  False ~=? puedeVolar puroContenedor,
  False ~=? puedeVolar tresCañones,
  False ~=? puedeVolar contenedorYCañon,
  False ~=? puedeVolar otroCañon,
  False ~=? puedeVolar escudoSinCañon,
  False ~=? puedeVolar protegido,
  True ~=? puedeVolar protegidoNivel1Estribor,
  True ~=? puedeVolar superProtegido,
  False ~=? puedeVolar desbalanceado,

  True ~=? mismoPotencial soloUnMotor nave1,
  False ~=? mismoPotencial puroContenedor tresCañones,
  True ~=? mismoPotencial nave2 nave3,
  True ~=? mismoPotencial contenedorYCañon otroCañon,
  True ~=? mismoPotencial nave4 nave5,
  False ~=? mismoPotencial escudoSinCañon protegido,
  True ~=? mismoPotencial nave6 nave7,
  False ~=? mismoPotencial protegidoNivel1Estribor superProtegido,
  False ~=? mismoPotencial superProtegido desbalanceado
  ]
  

testsEj3 = test [
  nave4 ~=? mayorCapacidad [nave4, nave2, nave3],
  nave5 ~=? mayorCapacidad [nave2, nave3, nave5],
  nave9 ~=? mayorCapacidad [nave7, nave8, nave9],
  puroContenedor ~=? mayorCapacidad [protegido, puroContenedor, contenedorYCañon],
  desbalanceado ~=? mayorCapacidad [desbalanceado, protegido, soloUnMotor]
  ]



testsEj4 = test [
  nave11 ~=? transformar (const Motor) naveEj12,
  tresCañones ~=? transformar (\comp -> if comp == Contenedor then Cañón else comp) puroContenedor,
  nave10 ~=? transformar (\comp -> case comp of Escudo -> Cañón
                                                Cañón -> Contenedor
                                                Contenedor -> Motor
                                                Motor -> Escudo) nave9
  ]

testsEj5 = test [
  -- Caso: Pequeño vs Escudo
  protegidoNivel1Estribor ~=? impactar (Estribor, 1, Pequeño) protegidoNivel1Estribor,
  -- Caso: Grande vs (Escudo + Cañon)
  protegidoNivel1Estribor ~=? impactar (Estribor, 1, Grande) protegidoNivel1Estribor,
  -- Caso: Torpedo vs X
  (Módulo Contenedor (Base Motor) (Base Contenedor)) ~=? impactar (Estribor, 1, Torpedo) protegidoNivel1Estribor,
  -- Caso: Pequeño/Grande/Torpedo vs (subNave sin proteccion)
  (Módulo Contenedor (Base Contenedor) protegido) ~=? impactar (Babor, 1, Pequeño) protegidoNivel1Estribor,
  -- Caso: Pequeño/Grande/Torpedo vs Cabina
  contenedorSolo ~=? impactar (Babor, 0, Grande) protegidoNivel1Estribor
  ]

testsEj6 = test [
  naveRestante ~=? maniobrar naveEj11 [(Babor, 1, Pequeño), (Estribor, 1, Torpedo), (Babor, 1, Grande), (Babor, 1, Torpedo)],
  naveChatarra ~=? maniobrar naveEj12 [(Babor, 1, Torpedo), (Estribor, 1, Torpedo)],
  naveChatarra ~=? maniobrar altaNave [(Babor, 1, Pequeño), (Babor, 1, Pequeño), (Estribor, 1, Torpedo), (Babor, 2, Grande), (Babor, 3, Torpedo), (Babor, 1, Torpedo)],
  naveNoMeDan ~=? maniobrar naveNoMeDan [(Babor, 1, Pequeño), (Estribor, 1, Pequeño), (Estribor, 1, Grande), (Babor, 1, Grande)]
  ]

testsEj7 = test [
  [nave1,nave3,nave9] ~=? pruebaDeFuego [(Babor,1,Grande),(Babor,2,Torpedo),(Estribor, 1, Pequeño)] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9],
  [naveNoMeDan] ~=? pruebaDeFuego [(Babor, 1, Pequeño), (Estribor, 1, Pequeño), (Estribor, 1, Grande), (Babor, 1, Grande), (Estribor, 1, Torpedo), (Babor, 1, Torpedo)] [naveNoMeDan,altaNave, naveEj11, naveIgualNoAndaba],
  [soloUnMotor, superProtegido] ~=? pruebaDeFuego [(Babor, 1, Pequeño), (Estribor, 1, Pequeño), (Estribor, 1, Grande), (Babor, 1, Grande), (Estribor, 1, Torpedo), (Babor, 1, Torpedo), (Babor, 2, Pequeño), (Estribor, 2, Pequeño), (Estribor, 2, Grande), (Babor, 2, Grande), (Estribor, 2, Torpedo), (Babor, 2, Torpedo)] [soloUnMotor, superProtegido, desbalanceado]
  ]

testsEj8 = test [
-- componentesPorNivel
  -- Caso: nave Base
  1 ~=? componentesPorNivel soloUnMotor 0,
  -- Caso: nave Modulo comp subNaveIzq subNaveDer
  4 ~=? componentesPorNivel nave4 2,
  -- Caso: nave con subNaves de distinta altura
  2 ~=? componentesPorNivel desbalanceado 2,
  -- Caso: nivel > (altura nave)
  0 ~=? componentesPorNivel protegidoNivel1Estribor 9,

-- dimensiones
  -- Caso: nave Base
  (1,1) ~=? dimensiones soloUnMotor,
  -- Caso: nave Modulo comp subNaveIzq subNaveDer
  (3,4) ~=? dimensiones nave4,
  -- Caso: nave con subNaves de distinta altura
  (4,4) ~=? dimensiones (Módulo Motor (Base Escudo) superProtegido),
  -- Caso: dimensiones de maniobrar/impactar nave
  (4,6) ~=? (dimensiones $ maniobrar nave9 [(Babor,1,Grande),(Babor,2,Torpedo)])
  ]

--Ejemplos de referencia para maniobrar:	
--maniobrar nave9 [(Babor, 0, Grande),(Babor,2,Torpedo),(Estribor,0,Pequeño)] destruye solo el subárbol izquierdo del subárbol izquierdo.
--maniobrar nave9 [(Estribor,0,Pequeño),(Babor,2,Torpedo),(Babor, 1, Grande)] destruye todo el subárbol izquierdo.