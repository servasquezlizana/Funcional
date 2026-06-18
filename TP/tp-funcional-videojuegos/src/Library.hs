module Library where
import PdePreludat

-- Data del videojuego
data VideoJuego = VideoJuego {
    titulo :: String,
    anioLanzamiento :: Number,
    listaExpanciones :: [String],
    precioBase :: Number
} deriving (Show)

-- Punto 1
impactoVJ :: VideoJuego -> Number
impactoVJ videojuego 
    |   ((< 1990) . anioLanzamiento) videojuego  && ((> 3) . length . listaExpanciones) videojuego  = precioBase videojuego * (length . listaExpanciones) videojuego
    |   ((< 1990) . anioLanzamiento) videojuego                                                     = ((+ ((*200) (1990 - anioLanzamiento videojuego))) . precioBase) videojuego
    |   ((== 0) . length . listaExpanciones) videojuego                                             = ((/2) . precioBase) videojuego
    |   otherwise                                                                                   =  precioBase videojuego + ((*50) . length . listaExpanciones) videojuego

-- Punto 2
-- Integrante 1
expansionDeCulto :: VideoJuego -> Bool
expansionDeCulto = (any ((elem "Director") . words)) . listaExpanciones

-- Punto 3
lanzarExpansion :: String -> VideoJuego -> VideoJuego
lanzarExpansion expansion videojuego =
    videojuego {
        listaExpanciones = listaExpanciones videojuego ++ [expansion],
        precioBase = precioBase videojuego + ((+100) . (*30) . length . listaExpanciones) videojuego
    }

-- Integrante 1
expancionesRestantes :: Number -> [String] -> [String]
expancionesRestantes longitud = (filter ((>=longitud) . length))

parchear :: Number -> VideoJuego -> VideoJuego
parchear longitud videojuego =
    videojuego {
        listaExpanciones = ((expancionesRestantes longitud) . listaExpanciones) videojuego,
        precioBase = precioBase videojuego + ((*100) . length . (expancionesRestantes longitud) . listaExpanciones) videojuego
    }