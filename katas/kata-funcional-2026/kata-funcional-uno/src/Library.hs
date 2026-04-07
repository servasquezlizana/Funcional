module Library where
import PdePreludat

esPalindromo :: String -> Bool
esPalindromo nombre = ((== nombre) . reverse) nombre

serieHeyArnold :: String -> Bool
serieHeyArnold = (== "Hey Arnold!")

heyArnoldOPalindromo :: String -> String -> Bool
heyArnoldOPalindromo nombre = ((|| esPalindromo nombre) . serieHeyArnold)

serieEsMasLargaQueNombre :: String -> String -> Bool
serieEsMasLargaQueNombre nombre = ((> length nombre) . length)

serieGravityFalls :: String -> Bool
serieGravityFalls = (== "Gravity Falls")

nivelDeCool :: String -> String -> Number
nivelDeCool nombre serieFav
    |   heyArnoldOPalindromo nombre serieFav     = 3
    |   serieEsMasLargaQueNombre nombre serieFav = 2
    |   serieGravityFalls serieFav               = 0
    |   otherwise                                = 1

-- la logica de serieGravityFalls y serieHeyArnold son la misma y se podria usar una funcion que 
-- eciba la serieFav y el nombre de la serie a comparar pero bajaria la cohesion de nivelDeCool, creooo
-- PD: Soy recursante