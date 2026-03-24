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
esMultiplo numero = (== 0) . mod numero

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
        | (== numero) numeroDivisor = listaNumeros ++ [numero]
        | esMultiplo numero numeroDivisor = buscarDivisores (numeroDivisor + 1) (listaNumeros ++ [numeroDivisor]) numero
        | otherwise = buscarDivisores (numeroDivisor + 1) listaNumeros numero

divisores :: Number -> [Number]
divisores = buscarDivisores 1 []

-- 8
exists :: (a -> Bool) -> [a] -> Bool
exists = any

-- 9
hayAlgunNegativo :: [Number] -> b -> Bool
hayAlgunNegativo listaNumeros b = any (<0) listaNumeros

-- 10
aplicarFunciones :: [Number -> Number] -> Number -> [Number]
aplicarFunciones funciones numero
  = foldr (\ funcion -> (:) (funcion numero)) [] funciones

{- 
aplicarFunciones :: [Number -> Number] -> Number -> [Number]
aplicarFunciones [] numero = []
aplicarFunciones (funcion:funciones) numero = funcion numero : aplicarFunciones funciones numero

Esta fue mi funcion la de arriba es una funcion que me recomendo haskell, por que no sé
-}

-- aplicarFunciones[(*4),even,abs] 8  Da error porque even devuleve un valor del tipo booleano y la funcion aplicar funciones esta definida con un codominio de listas de numeros

-- 11
sumaF :: [Number -> Number] -> Number -> Number
sumaF funciones = sum . aplicarFunciones funciones

-- 12
subirHabilidad :: Number -> [Number] -> [Number]
subirHabilidad numero [] = []
subirHabilidad numero (habilidad:habilidades)
    | ((>= 12) . (+ numero)) habilidad  = 12 : subirHabilidad numero habilidades
    | otherwise = (+ numero) habilidad : subirHabilidad numero habilidades

-- 13
flimitada :: (Number -> Number) -> Number -> Number
flimitada funcion = min 12 . max 0 . funcion

-- 13 a
cambiarHabilidad :: (Number -> Number) -> [Number] -> [Number]
cambiarHabilidad funcion = map (flimitada funcion)

-- 13 b esta en los tests   ->  cambiarHabilidad (max 4) [2,4,5,3,8] `shouldBe` [4,4,5,4,8] 

-- 14 esta en los tests

-- 15
primerosPares :: [Number] -> [Number]
primerosPares = takeWhile even

primerosDivisores :: Number -> [Number] -> [Number]
primerosDivisores numero = takeWhile (esMultiplo numero)

primerosNoDivisores :: Number -> [Number] -> [Number]
primerosNoDivisores numero = takeWhile (not . esMultiplo numero)

-- 16
algunoMayorA :: Number -> [Number] -> Bool
algunoMayorA numero = any (numero <)

utilidad :: [Number] -> [Number] -> [Number]
utilidad [] [] = []
utilidad listaIngresos listaEgresos = head listaIngresos - head listaEgresos : utilidad (tail listaIngresos) (tail listaEgresos)

huboMesMejorDe :: [Number] -> [Number] -> Number -> Bool
huboMesMejorDe listaIngresos listaEgresos numero = algunoMayorA numero (utilidad listaIngresos listaEgresos)

-- 17 a
muchosDe :: Number -> [Number]
muchosDe n = n:muchosDe n

crecimientoEstadisticoAnual :: [Number]
crecimientoEstadisticoAnual = [22,20..4] ++ [4,4,4,4,4,4,2,2,1,1] ++ muchosDe 0

crecimientoAnual :: Number -> Number
crecimientoAnual edad = (!!) crecimientoEstadisticoAnual (edad - 1)

-- 17 b
{-
crecimientoEntreEdades :: Number -> Number -> Number
crecimientoEntreEdades edadInferior edadSuperior
    | edadInferior == edadSuperior = 0
    | otherwise = crecimientoAnual edadInferior + crecimientoEntreEdades (edadInferior + 1) edadSuperior
-}

crecimientoEntreEdades :: Number -> Number -> Number
crecimientoEntreEdades edadInferior edadSuperior = (sum . take ((-) edadSuperior edadInferior) . drop ((-) edadInferior 1)) crecimientoEstadisticoAnual

-- 17 c
alturasEn1Anio :: Number -> [Number] -> [Number]
alturasEn1Anio edad = map (+ crecimientoAnual edad)

-- 17 d
alturaEnEdades :: Number -> Number -> [Number] -> [Number]
alturaEnEdades altura edad = map ((+ altura) . crecimientoEntreEdades edad)

-- 18
lluviasEnero :: [Number]
lluviasEnero = [0,2,5,1,34,2,0,21,0,0,0,5,9,18,4,0]

rachasLluvia :: [Number] -> [[Number]]
rachasLluvia [] = []
rachasLluvia listaLluvias
    |   ((> 0) . head) listaLluvias = takeWhile (> 0) listaLluvias : (rachasLluvia . dropWhile (> 0)) listaLluvias
    | otherwise = (rachasLluvia . dropWhile (== 0)) listaLluvias

mayorRachasDeLluvia :: [Number] -> Number
mayorRachasDeLluvia = maximum . map length . rachasLluvia

-- 19   sumatoria = foldl1 (+)
sumatoria :: [Number] -> Number
sumatoria = foldr1 (+)

-- 20   productoria = foldl1 (*)
productoria :: [Number] -> Number
productoria = foldr1 (*)

-- 21 PREGUNTAR QUE ES DISPERSION