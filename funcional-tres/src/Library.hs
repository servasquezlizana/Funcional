module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

{- Funcional 3: Tuplas -}
-- 1
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

-- 2
aplicar :: (Number -> Number, Number -> Number) -> Number -> (Number, Number)
aplicar (a, b) numero = (a numero, b numero)

-- 3
cuentaBizarra :: (Number, Number) -> Number
cuentaBizarra (primerNumero, segundoNumero)
    |   primerNumero > segundoNumero = primerNumero + segundoNumero
    |   segundoNumero - primerNumero >= 10 = segundoNumero - primerNumero
    |    segundoNumero - primerNumero > 1 = primerNumero * segundoNumero

-- 4
promedioNotas :: (Number , Number) -> Number
promedioNotas (nota1, nota2)= (/2) ((+) nota1 nota2)

esNotaBochorno :: Number -> Bool
esNotaBochorno = (< 6)

aprobo :: (Number, Number) -> Bool
aprobo = not . esNotaBochorno . promedioNotas

promociono :: (Number , Number) -> Bool
promociono (nota1, nota2) = nota1 + nota2 >= 15 && (>=7) (min nota1 nota2)

aproboPrimerParcial :: (Number , Number) -> Bool
aproboPrimerParcial (notas1, _) = (not . esNotaBochorno) notas1

-- 5
apruebaAnio :: (Number, Number) -> Bool
apruebaAnio (nota1, nota2) =  6 <= min nota1 nota2

notasFinales :: ((Number , Number),(Number , Number)) -> (Number , Number)
notasFinales ((notas1,notas2),(recu1, recu2)) = (max notas1 recu1, max notas2 recu2)

recursa :: ((Number , Number),(Number , Number)) -> Bool
recursa = not . apruebaAnio . notasFinales

recupero :: ((Number , Number),(Number , Number)) -> Bool
recupero (_, (recu1,_)) = (> (-1)) recu1

recuperoDeGusto :: ((Number , Number),(Number , Number)) -> Bool
recuperoDeGusto (notas, (recu1, recu2))
    | promociono notas = -1 < max recu1 recu2
    | otherwise  = False

-- 6
esMayorDeEdad :: (String, Number) -> Bool
esMayorDeEdad (_, edad) = (>= 18) edad

-- 7
operacionPrimerElemento :: Number -> Number
operacionPrimerElemento primerElemento
    | even primerElemento = 2 * primerElemento
    | otherwise = primerElemento

operacionSegundoElemento :: Number -> Number
operacionSegundoElemento segundoElemento
    | odd segundoElemento = 1 + segundoElemento
    | otherwise = segundoElemento

calcular :: (Number, Number) -> (Number, Number)
calcular (numero1, numero2) = (operacionPrimerElemento numero1, operacionSegundoElemento numero2)