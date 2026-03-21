module Library where
import PdePreludat
import GHC.Base (Float)

doble :: Number -> Number
doble numero = numero + numero


{- Funcional 2: Aplicación Parcial y Composición -}

-- Aplicacion Parcial
-- 1
siguiente :: Number -> Number
siguiente = (+ 1)

-- 2
mitad :: Number -> Number
mitad = (/ 2)

-- 3
inversa :: Number -> Number
inversa = (^^ (-1))

-- 4
triple :: Number -> Number
triple = (* 3)

-- 5
esNumeroPositivo :: Number -> Bool
esNumeroPositivo = (> 0)

-- Composicion
-- 6
esMultiploDe :: Number -> Number -> Bool
esMultiploDe = ((==0) .) . flip mod

-- 7
esBisiesto :: Number -> Bool
esBisiesto anio = (esMultiploDe 400 anio || esMultiploDe 4 anio) && (not . esMultiploDe 100) anio

-- 8
inversaRaizCuadada :: Number -> Number
inversaRaizCuadada = inversa . sqrt 

-- 10
incrementMCuadradoN :: Number -> Number -> Number
incrementMCuadradoN m n = (+) n (m^2)
--incrementMCuadradoN = (. (^2)) . (+)

-- 11
esResultadoPar :: Number -> Number -> Bool
esResultadoPar m n = even (m^n)
