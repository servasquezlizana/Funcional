module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Partido = (Number, Number)

minutosJugados :: Partido -> Number
minutosJugados = fst

golesConvertidos :: Partido -> Number
golesConvertidos = snd

data Jugador = Jugador
    {
    nombre :: String,
    velocidad :: Number,
    habilidad :: Number,
    posicion :: String,
    partidos :: [Partido]
    }

-- Punto 1: Equipos

type Equipo = [Jugador]

{-
jugadoresHabituales :: Equipo -> Number -> [String]
jugadoresHabituales equipo minutos = map nombre (filter (\ jugador -> all ((>= minutos) . minutosJugados) (partidos jugador)) equipo)

jugadoresGoleadores :: Equipo -> Number
jugadoresGoleadores equipo = (length . filter (\ jugador -> all ((> 0) . golesConvertidos) (partidos jugador))) equipo

volantesConHabilidad :: Equipo -> Number -> Bool
volantesConHabilidad equipo skill = all (\ jugador -> (== "volante") (posicion jugador) && (>= skill) (habilidad jugador)) equipo
-}

-- ****** Corregido ******
-- Punto 1 (Sin lambdas, puro point-free)
jugadoresHabituales :: Equipo -> Number -> [String]
jugadoresHabituales equipo minutos = map nombre . filter (all ((>= minutos) . minutosJugados) . partidos) $ equipo

jugadoresGoleadores :: Equipo -> Number
jugadoresGoleadores = length . filter (all ((> 0) . golesConvertidos) . partidos)

volantesConHabilidad :: Equipo -> Number -> Bool
volantesConHabilidad equipo skill = all ((== "volante") . posicion) . filter ((>= skill) . habilidad) $ equipo

-- Punto 2: Tecnicos
type Entrenador = Jugador -> Jugador 

    -- Bielsa
incrementarVelocidad :: Number -> Jugador -> Jugador 
incrementarVelocidad modificador jugador = jugador { velocidad = ((* modificador) . velocidad) jugador}

cambiarHabilidad :: (Number -> Number) -> Jugador -> Jugador 
cambiarHabilidad decremento jugador = jugador { habilidad = (decremento . habilidad) jugador}

{-
bielsa :: Entrenador
bielsa = ((cambiarHabilidad (10-)) . (incrementarVelocidad 1.5))
-}
-- ****** Corregido ******
bielsa :: Entrenador
bielsa = cambiarHabilidad (subtract 10) . incrementarVelocidad 1.5

    -- Gago
cambiarPosicion :: String -> Jugador -> Jugador 
cambiarPosicion modificador jugador = jugador { posicion = modificador}

cambiaVolantesDelanteros :: Jugador -> Jugador
cambiaVolantesDelanteros jugador | ((== "volante") . posicion) jugador = cambiarPosicion "defensor" jugador
                                 | ((== "delantero") . posicion) jugador = cambiarPosicion "volante" jugador 
gago :: Entrenador
gago = cambiaVolantesDelanteros

    -- Menotti
cambiosDeMenotti :: (Number -> Number) -> Jugador -> Jugador
cambiosDeMenotti cambio jugador  = (cambiarHabilidad cambio jugador){nombre = "Mr." ++ nombre jugador}

menotti :: (Number -> Number) -> Entrenador
menotti = cambiosDeMenotti

    -- Bertolotti
bertolotti :: Entrenador
bertolotti = menotti (+10)

    -- Van Gaal
vanGaal :: Entrenador
vanGaal = id

    -- Entrenar Equipo
dirigeDT :: Entrenador -> Equipo -> Equipo
dirigeDT = map 

-- >  (vanGaal . menotti (+10) . bielsa) unJugador

-- Punto 3: Mejora
esBueno :: Jugador -> Bool
esBueno jugador = habilidad jugador > velocidad jugador || ((== "volante") . posicion) jugador

esBuenDT :: Entrenador -> Equipo -> Bool
esBuenDT entrenador equipo = (<) (cantidadDeBuenos equipo) (cantidadDeBuenos (dirigeDT entrenador equipo))
    where cantidadDeBuenos = length . filter esBueno

esJugadorBueno :: [Entrenador] -> Jugador -> Bool
esJugadorBueno entrenadores jugador = esBueno jugadorExperimentado
    where jugadorExperimentado = foldl (\jugador entrenador -> entrenador jugador) jugador entrenadores

-- Punto 4: Es imparable
esGoleadorImparable :: [Partido] -> Bool
esGoleadorImparable [] = False
esGoleadorImparable [_] = True
esGoleadorImparable (partido1:partido2:partidos) = golesConvertidos partido1 <= golesConvertidos partido2 && esGoleadorImparable (partido2:partidos)

esImparable :: Jugador -> Bool
esImparable = (esGoleadorImparable . partidos)