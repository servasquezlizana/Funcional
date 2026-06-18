module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Opinion = String
type Reparacion = (Number,String)

tiempoReparacion :: Reparacion -> Number
tiempoReparacion = fst

trabajoReparacion :: Reparacion -> String
trabajoReparacion = snd

data Atraccion = Atraccion 
    {
        nombre          :: String,
        alturaMinima    :: Number, -- en centimetros
        duracion        :: Number, -- en minutos
        opiniones       :: [Opinion],
        enMantenimiento :: Bool,
        reparaciones    :: [Reparacion]
    }


-- Punto 1: Mas bueno que ...

esProlongada :: Atraccion -> Bool
esProlongada = ((>10).duracion)

tieneReparaciones :: Number -> Atraccion -> Bool
tieneReparaciones numero = ((<= numero).length.reparaciones)

atraccionBuenaScoring :: Atraccion -> Number
atraccionBuenaScoring atraccion | esProlongada atraccion           = 100
                                | tieneReparaciones 3 atraccion    =  ((+2).length.nombre) atraccion
                                | otherwise                        = ((*10).alturaMinima) atraccion

-- Punto 2: Iguana fisssssssssssss
type Restoration = Atraccion -> Atraccion

pasaPorRepaciones :: Restoration
pasaPorRepaciones atraccion = atraccion 
    {
        reparaciones    = reparacionesRestantes,
        enMantenimiento = (not . null) reparacionesRestantes
    } where reparacionesRestantes = take (((subtract 1).length.reparaciones) atraccion) (reparaciones atraccion)

ajusteDeTornilleria :: Number -> Restoration
ajusteDeTornilleria tornillos atraccion = (pasaPorRepaciones atraccion) { duracion = min 10 (((+tornillos).duracion) atraccion) }

engrase :: Number -> Restoration
engrase gramos atraccion = (pasaPorRepaciones atraccion)
    { 
        alturaMinima = ((+0.1*gramos).alturaMinima) atraccion,
        opiniones = (opiniones atraccion) ++ ["para valientes"]
    }

mantenimientoElectrico :: Restoration
mantenimientoElectrico atraccion = (pasaPorRepaciones atraccion) { opiniones = (take 2 . opiniones) atraccion }

mantenimientoBasico :: Restoration
mantenimientoBasico = engrase 10 . ajusteDeTornilleria 8

-- Punto 3: ¿Que oooooooooooonda este parque?

type Parque = [Atraccion] 

meDaMiesdito :: Atraccion -> Bool
meDaMiesdito atraccion = any ((>=4).tiempoReparacion) (reparaciones atraccion)

acaCerramos :: Atraccion -> Bool
acaCerramos atraccion = ((>=7) . sumOf tiempoReparacion) (reparaciones atraccion)


tieneNombreCheto :: Atraccion -> Bool
tieneNombreCheto = (>=5).length.nombre

disneyNoEsistis :: Parque -> Bool
disneyNoEsistis = all (null . reparaciones) . filter tieneNombreCheto

-- Punto 4: Reparaciones peolas
reparacionesPeolas :: [Restoration] -> Atraccion -> Bool
reparacionesPeolas [] _                                     = False
reparacionesPeolas [reparacion1] atraccion                  = atraccionBuenaScoring atraccion <= (atraccionBuenaScoring . reparacion1) atraccion
reparacionesPeolas (reparacion1:reparacion2:masReparaciones) atraccion  = (atraccionBuenaScoring . reparacion1) atraccion <= (atraccionBuenaScoring . reparacion2. reparacion1) atraccion && reparacionesPeolas (reparacion2:masReparaciones) (reparacion1 atraccion)

-- Punto 5: Manny a la obra
realizarReparaciones :: [Restoration] -> Atraccion -> Atraccion
realizarReparaciones muchasreparaciones atraccion = foldl (\atraccion reparacion -> reparacion atraccion) atraccion muchasreparaciones

-- > realizarReparaciones [lista de reparaciones] unaAtraccion

-- Punto 6: Estoy cansado jefe...
-- Aca respondo con lazy evaluation