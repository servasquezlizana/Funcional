{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Hoist not" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

{- Funcional 4: Listas + Orden Superior -}

-- Listas
-- 1
summ :: [Number] -> Number -> Number
summ listaNumeros totalLista
    | (not . null) listaNumeros = summ (tail listaNumeros) (totalLista + head listaNumeros)
    | otherwise = totalLista

sumarListaNumeros :: [Number] -> Number
sumarListaNumeros listaNumeros = summ listaNumeros 0

-- 2. a
promedioFrecuenciaCardiaca :: [Number] -> Number
promedioFrecuenciaCardiaca listaFrecuencia = sumarListaNumeros listaFrecuencia / length listaFrecuencia

-- b
frecuenciaCardiaca :: [Number]
frecuenciaCardiaca = [80, 100, 120, 127, 130, 123, 125]

lugarMinuto :: Number -> Number
lugarMinuto = (/10)

frecuenciaCardiacaMinuto :: Number -> Number
frecuenciaCardiacaMinuto minuto = (!!) frecuenciaCardiaca (lugarMinuto minuto)

-- c
frecuenciaHastaMomento :: Number -> [Number]
frecuenciaHastaMomento minuto = take (((+ 1) . lugarMinuto) minuto) frecuenciaCardiaca

-- 3
esCapicua :: [String] -> Bool
esCapicua listaSilabas = (reverse . concat) listaSilabas == concat listaSilabas

-- 4
duracionLlamadas :: ((String, [Number]), (String, [Number]))
duracionLlamadas = (("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10]))

masMinutos :: ((String, [Number]), (String, [Number])) -> String
masMinutos ((horarioReducido, tiemposLlamadasReducido), (horarioNormal, tiemposLlamadasNormal))
    | sumarListaNumeros tiemposLlamadasReducido > sumarListaNumeros tiemposLlamadasNormal = horarioReducido
    | otherwise = horarioNormal

cuandoHabloMasMinutos :: String
cuandoHabloMasMinutos = masMinutos duracionLlamadas

-- Orden Superior
-- 1
existsAny :: (a -> Bool) -> (a,a,a) -> Bool
existsAny funcion (a,b,c) = funcion a || funcion b || funcion c

-- 2
mejor :: (Number -> Number) -> (Number -> Number) -> Number -> Number
mejor funcion1 funcion2 numero = max (funcion1 numero) (funcion2 numero)

-- 3
aplicarPar :: (a -> b) -> (a,a) -> (b,b)
aplicarPar funcion (a,b) = (funcion a, funcion b)

-- 4
parDeFns :: (a -> b) -> (a -> c) -> a -> (b,c)
parDeFns funcion1 funcion2 elemento = (funcion1 elemento, funcion2 elemento)

-- Orden Superior + Lista
-- 1
esMultiplo :: Number -> Number -> Bool
esMultiplo multiplo numero = mod numero multiplo == 0

esMultiploDeAlguno :: Number -> [Number] -> Bool
esMultiploDeAlguno multiplo = any (esMultiplo multiplo)

-- 2
promedioListaNumeros :: [Number] -> Number
promedioListaNumeros listaNumeros = (/ length listaNumeros) (sumarListaNumeros listaNumeros)

promedios :: [[Number]] -> [Number]
promedios = map promedioListaNumeros

-- 3
filtrarListasMenoresA :: Number -> [Number] -> [Number]
filtrarListasMenoresA numero = filter (numero<)

promediosSinAplazos :: [[Number]] -> [Number]
promediosSinAplazos =  promedios . map (filtrarListasMenoresA 4)

-- 4
mejoresNotas :: [[Number]] -> [Number]
mejoresNotas = map maximum

-- 5
aprobo :: [Number] -> Bool
aprobo = (6<=) . minimum

-- 6
aprobaron :: [[Number]] -> [[Number]]
aprobaron = filter aprobo

-- 7
buscarDivisores :: Number -> [Number] -> Number -> [Number]
buscarDivisores numeroDivisor listaNumeros numero
        | numeroDivisor == numero = listaNumeros ++ [numero]
        | esMultiplo numeroDivisor numero = buscarDivisores (numeroDivisor + 1) (listaNumeros ++ [numeroDivisor]) numero
        | otherwise = buscarDivisores (numeroDivisor + 1) listaNumeros numero

divisores :: Number -> [Number]
divisores = buscarDivisores 1 []