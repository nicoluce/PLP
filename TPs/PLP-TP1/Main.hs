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
  3 ~=? capacidad puroContenedor
  ]

testsEj3 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj4 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
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
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj7 = test [
  [nave1,nave3,nave9] ~=? pruebaDeFuego [(Babor,1,Grande),(Babor,2,Torpedo),(Estribor, 1, Pequeño)] [nave1,nave2,nave3,nave4,nave5,nave6,nave7,nave8,nave9]
  ]

testsEj8 = test [
  -- Caso: nave Base
  1 ~=? componentesPorNivel soloUnMotor 0,
  -- Caso: nave Modulo comp subNaveIzq subNaveDer
  4 ~=? componentesPorNivel nave4 2,
  -- Caso: nave con subNaves de distinta altura
  2 ~=? componentesPorNivel desbalanceado 2,
  -- Caso: nivel > (altura nave)
  0 ~=? componentesPorNivel protegidoNivel1Estribor 9,
  (4,6) ~=? (dimensiones $ maniobrar nave9 [(Babor,1,Grande),(Babor,2,Torpedo)])
  ]

--Ejemplos de referencia para maniobrar:	
--maniobrar nave9 [(Babor, 0, Grande),(Babor,2,Torpedo),(Estribor,0,Pequeño)] destruye solo el subárbol izquierdo del subárbol izquierdo.
--maniobrar nave9 [(Estribor,0,Pequeño),(Babor,2,Torpedo),(Babor, 1, Grande)] destruye todo el subárbol izquierdo.