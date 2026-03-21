{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

{- Funcional 1: Primeros ejercicios -}

-- 1
esMultiploDeTres :: Number -> Bool
esMultiploDeTres numero = mod numero 3 == 0

-- 2
esMultiploDe :: Number -> Number -> Bool
esMultiploDe numero1 numero2 = mod numero2 numero1 == 0

-- 3
cubo :: Number -> Number
cubo numero = numero * numero * numero

-- 4
area :: Number -> Number -> Number
area basee alturaa = basee * alturaa

-- 5
esBisiesto :: Number -> Bool
esBisiesto anio = (esMultiploDe 400 anio || esMultiploDe 4 anio) && not (esMultiploDe 100 anio)

-- 6
celciusToFahr :: Number -> Number
celciusToFahr temperatura = (temperatura * (9/5)) + 32

-- 7
fahrToCelcius :: Number -> Number
fahrToCelcius temperatura = (temperatura - 32) / (9/5)

-- 8
haceFrioF :: Number -> Bool
haceFrioF temperaturaF = ((fahrToCelcius temperaturaF) < 8)

-- 9
mcm :: Number -> Number -> Number
mcm a b = (a * b) / gcd a b

-- 10
dispersion :: Number -> Number -> Number -> Number
dispersion medicion1 medicion2 medicion3 = max medicion1 (max medicion2 medicion3) - min medicion1 (min medicion2 medicion3)

diasParejos :: Number -> Number -> Number -> Bool
diasParejos medicion1 medicion2 medicion3 = 30 >= dispersion medicion1 medicion2 medicion3

diasLocos :: Number -> Number -> Number -> Bool
diasLocos medicion1 medicion2 medicion3 = 100 < dispersion medicion1 medicion2 medicion3

diasNormales :: Number -> Number -> Number -> Bool
diasNormales medicion1 medicion2 medicion3 = not (diasParejos medicion1 medicion2 medicion3) && not (diasLocos medicion1 medicion2 medicion3)

-- 11
pesoPino :: Number -> Number
pesoPino altura | altura < 300   = altura * 3
                | otherwise      = altura * 2

esPesoUtil :: Number -> Bool
esPesoUtil peso = peso < 1000 && peso > 400

sirvePino :: Number -> Bool
sirvePino = esPesoUtil . pesoPino

-- 12
cuadradoPerfecto :: Number -> Number -> Bool
cuadradoPerfecto n numero
    | n * n == numero   = True
    | n * n > numero    = False
    | otherwise         = cuadradoPerfecto (n + 1) numero

esCuadradoPerfecto :: Number -> Bool
esCuadradoPerfecto = cuadradoPerfecto 0