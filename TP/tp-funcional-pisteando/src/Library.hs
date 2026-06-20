module Library where
import PdePreludat

{- ===================== ENTREGA 1 ===================== -}
-- 1. Data autos
data Auto = Auto {
    marca :: String,
    modelo :: String,
    desgasteRuedas :: Number,
    desgasteChasis :: Number,
    velocidadMaxima :: Number,
    apodos :: [String],
    tiempoCarrera :: Number
} deriving (Show)

-- 2. Estado de salud del auto
esPeugeot :: Auto -> Bool
esPeugeot = (== "Peugeot") . marca

pocoTiempoEnPista :: Auto -> Bool
pocoTiempoEnPista = (< 100) . tiempoCarrera

estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado auto 
                | esPeugeot auto            = False
                | pocoTiempoEnPista auto    = ((< 20) . desgasteChasis) auto
                | otherwise                 = ((< 40) . desgasteChasis) auto && ((< 60) . desgasteRuedas) auto

primerApodo :: [String] -> String
primerApodo = head

primeraPalabra :: [String] -> String
primeraPalabra = head

noDaMas :: Auto -> Bool
noDaMas auto
    | empiezaConLa (head (apodos auto)) = desgasteChasis auto > 80
    | otherwise                         = desgasteRuedas auto > 80
  where
    empiezaConLa apodo = take 3 apodo == "La "

esChiche :: Auto -> Bool
esChiche auto
    | even (length (apodos auto)) = desgasteChasis auto < 20
    | otherwise                   = desgasteChasis auto < 50

esJoya :: Auto -> Bool
esJoya auto = ((== 0) . desgasteChasis) auto && ((== 0) . desgasteRuedas) auto && ((< 2).length.apodos) auto

nivelDeChetez :: Auto -> Number
nivelDeChetez auto = ((* 20).(length.apodos)) auto * ((length.modelo) auto)

capacidadSupercalifragilisticaespialidosa :: Auto -> Number
capacidadSupercalifragilisticaespialidosa auto = (length.primerApodo.apodos) auto

riesgoso :: Auto -> Number
riesgoso auto 
                | estaEnBuenEstado auto = (velocidadMaxima auto) * (desgasteRuedas auto / 10)
                | otherwise             = (velocidadMaxima auto) * (desgasteRuedas auto / 10) * 2

-- 3. Manos a la obra

reparar :: Auto -> Auto
reparar auto = auto 
    {
        desgasteChasis = ((* 0.15).desgasteChasis) auto,
        desgasteRuedas = 0
    }

penalizar :: Number -> Auto -> Auto
penalizar penalidad auto = auto { tiempoCarrera = ((+ penalidad).tiempoCarrera) auto }

ponerNitro :: Auto -> Auto
ponerNitro auto = auto { velocidadMaxima = ((* 1.2).velocidadMaxima) auto }

bautizar :: String -> Auto -> Auto
bautizar apodonuevo auto = auto { apodos = apodos auto ++ [apodonuevo] }

desarmadero :: String -> String -> Auto -> Auto
desarmadero marcanueva modelonuevo auto = auto 
    {
        marca = marcanueva,
        modelo = modelonuevo,
        apodos = ["Nunca Taxi"]
    }

-- 4. Pistas
type Tramo = Auto -> Auto
data Pista = Pista 
    {
        nombre :: String,
        pais :: String,
        precioEntrada :: Number,
        tramos :: [Tramo]
    } deriving (Show)

modificoTiempo :: (Number -> Number) -> Tramo
modificoTiempo funcionModificadora auto = auto { tiempoCarrera = (funcionModificadora.tiempoCarrera) auto }

modificoDesgasteRuedas :: (Number -> Number) -> Tramo
modificoDesgasteRuedas funcionModificadora auto = auto { desgasteRuedas = (funcionModificadora.desgasteRuedas) auto}

modificoDesgasteChasis :: (Number -> Number) -> Tramo
modificoDesgasteChasis funcionModificadora auto = auto { desgasteChasis = (funcionModificadora.desgasteChasis) auto}

curva :: Number -> Number -> Tramo
curva angulo longitud auto = (modificoDesgasteRuedas (+ 3 * longitud / angulo) . modificoTiempo (+ longitud / ( velocidadMaxima auto / 2 ))) auto

curvaPeligrosa :: Tramo
curvaPeligrosa = curva 60 300

curvaTranca :: Tramo
curvaTranca = curva 110 550

tramoRecto :: Number -> Tramo
tramoRecto distancia auto = (modificoTiempo (+ (distancia / velocidadMaxima auto)) . modificoDesgasteChasis (+ (distancia / 100))) auto

tramoRectoClassic :: Tramo
tramoRectoClassic = tramoRecto 715

tramito :: Tramo
tramito = tramoRecto 260

zigzag :: Number -> Tramo
zigzag cambios auto = (modificoDesgasteChasis (+ 5) . modificoDesgasteRuedas (+ (velocidadMaxima auto * cambios / 10)) . modificoTiempo (+ 3 * cambios)) auto

zigzagLoco :: Tramo
zigzagLoco = zigzag 5

casiCurva :: Tramo
casiCurva = zigzag 1

rulo :: Number -> Tramo
rulo diametro auto = (modificoTiempo (+ 5 * diametro / velocidadMaxima auto) . modificoDesgasteRuedas (+ diametro * 1.5)) auto

ruloClasico :: Tramo
ruloClasico = rulo 13

deseoDeMuerte :: Tramo
deseoDeMuerte = rulo 26

nivelJoya :: Auto -> Number
nivelJoya auto
    | tiempoCarrera auto < 50 = 1
    | otherwise               = 2

nivelDeJoyez :: [Auto] -> Number
nivelDeJoyez = sumOf nivelJoya

paraEntendidos :: [Auto] -> Bool
paraEntendidos []           = True
paraEntendidos (auto:autos) = estaEnBuenEstado auto && tiempoCarrera auto <= 200 && paraEntendidos autos

redondearA2Decimales :: Number -> Number
redondearA2Decimales numero = round (numero * 100) / 100

{- ===================== ENTREGA 2 ===================== -}

data Equipo = Equipo
    {
    nombreEquipo :: Number,
    autos :: [Auto],
    presupuesto :: Number
    } deriving (Show)


agregarAuto :: Equipo -> Auto -> Equipo
agregarAuto equipo auto 
                        | presupuesto equipo > costoInscripcion = equipo
                                                                        {
                                                                            autos = autos equipo ++ [auto],
                                                                            presupuesto = (presupuesto equipo) - costoInscripcion
                                                                        }
                        | otherwise                             = equipo
                        where costoInscripcion = ((* 1000).velocidadMaxima) auto


cantidadReducida :: (b -> Number) -> b -> (b -> b) -> Number
cantidadReducida desgaste auto funcion = (-) ((desgaste.funcion) auto) (desgaste auto) 

repararAutos :: Equipo -> [Auto] -> Equipo
repararAutos equipo [] = equipo
repararAutos equipo (auto:restoAutos)
    | presupuesto equipo >= costoReparacion = repararAutos equipoActualizado restoAutos
    | otherwise                             = repararAutos equipo restoAutos -- No alcanza, pasa al siguiente auto
  where
    costoReparacion   = cantidadReducida desgasteChasis auto reparar * 500
    equipoActualizado = equipo 
        { 
            presupuesto = presupuesto equipo - costoReparacion,
            autos = reparar auto : autos equipo 
        }

repararEquipo :: Equipo -> Equipo
repararEquipo equipo = repararAutos equipo (autos equipo)

nitroAutos :: Equipo -> [Auto] -> Equipo
nitroAutos equipo [] = equipo
nitroAutos equipo (auto:restoAutos)
    | presupuesto equipo >= costoReparacion = nitroAutos equipoActualizado restoAutos
    | otherwise                             = nitroAutos equipo restoAutos -- No alcanza, pasa al siguiente auto
  where
    costoReparacion   = velocidadMaxima auto * 100
    equipoActualizado = equipo 
        { 
            presupuesto = presupuesto equipo - costoReparacion,
            autos = ponerNitro auto : autos equipo 
        }

nitroEquipo :: Equipo -> Equipo
nitroEquipo equipo = nitroAutos equipo (autos equipo)

ferrarizarAutos :: Equipo -> [Auto] -> Equipo
ferrarizarAutos equipo [] = equipo
ferrarizarAutos equipo (auto:restoAutos)
    | (presupuesto equipo >= costoReparacion) && (not.(== "Ferrari").marca) auto = ferrarizarAutos equipoActualizado restoAutos
    | otherwise                                                                  = ferrarizarAutos equipo restoAutos -- No alcanza, pasa al siguiente auto
  where
    costoReparacion   = 3500
    equipoActualizado = equipo 
        { 
            presupuesto = presupuesto equipo - costoReparacion,
            autos = desarmadero "Ferrari" "F90" auto : autos equipo 
        }

ferrarizarEquipo :: Equipo -> Equipo
ferrarizarEquipo equipo = ferrarizarAutos equipo (autos equipo)

