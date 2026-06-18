module Library where
import PdePreludat

-- data autos
data Auto = Auto {
    marca :: String,
    modelo :: String,
    desgasteRuedas :: Number,
    desgasteChasis :: Number,
    velocidadMaxima :: Number,
    listaApodos :: [String],
    tiempoEnPista :: Number
}

estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado auto 
    | ((== "Peugeot") . marca) auto     = False
    | ((< 100) . tiempoEnPista) auto    = ((< 20) . desgasteChasis) auto
    | otherwise                         = ((< 40) . desgasteChasis) auto && ((< 60) . desgasteRuedas) auto

noDaMas :: Auto -> Bool
noDaMas auto 