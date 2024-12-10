module Library where
import PdePreludat

data Cancion = UnaCancion {
    titulo :: String,
    duracion :: Number,
    instrumentos :: [Instrumento]
}deriving(Show, Eq)

-- Multiples constructores (un booleano con varias opciones)
data Instrumento = Guitarra | Bajo | Bateria | Teclado | Saxofon deriving(Show, Eq)

-- Por el momento la banda cuenta con algunas canciones...

patternMatching :: Cancion
patternMatching = UnaCancion "Pattern Matching" 4 [Guitarra, Bajo, Bateria]

seisDieciocho :: Cancion
seisDieciocho = UnaCancion "Seis dieciocho" 3 [Teclado, Guitarra]

laVidaEnHaskell :: Cancion
laVidaEnHaskell = UnaCancion "La vida en Haskell" 5 []

-- Aceptacion

aceptacion :: Cancion -> Number
aceptacion cancion 
    | comienzaConM cancion = 500
    | duracionPar cancion = length (titulo cancion) * 10  -- ((*10) . length . titulo) cancion 
    | esAcapella cancion = 10
    | otherwise = 0

comienzaConM :: Cancion -> Bool
comienzaConM cancion = head (titulo cancion) == 'M'

duracionPar :: Cancion -> Bool
duracionPar cancion = even (duracion cancion)

-- Repertorio

type Repertorio = [Cancion]

repertorio :: Repertorio
repertorio = [patternMatching, seisDieciocho, laVidaEnHaskell, haskellEsAmor, melodiasFuncionales]

-- 1) Definir al menos 2 canciones más para la banda

haskellEsAmor :: Cancion
haskellEsAmor = UnaCancion "Haskell es amor" 6 [Saxofon, Bajo, Bateria]

melodiasFuncionales :: Cancion
melodiasFuncionales = UnaCancion "Melodias Funcionales" 2 [Guitarra]

-- 2) PdePop tiene la costumbre de tocar sus canciones por orden alfabético. 
-- Dadas dos canciones, determinar cuál viene antes en el repertorio.

vieneAntes :: Cancion -> Cancion -> Cancion 
vieneAntes cancion1 cancion2 
    | titulo cancion1 < titulo cancion2 = cancion1
    | otherwise = cancion2

-- Es al dope agarrar la primer letra, ya haskell hace la comparacion entre titulos directamente
--comienzaCon :: Cancion -> Char
--comienzaCon cancion = head (titulo cancion)

-- 3) Determinar si una canción es acapella, es decir, no se utilizan instrumentos en su interpretación. 

esAcapella :: Cancion -> Bool
esAcapella cancion = null (instrumentos cancion)

-- 4) Averiguar si una canción es aceptada por el público, esto ocurre cuando su índice de aceptación es mayor a 60.

aceptadaPorElPublico :: Cancion -> Bool
aceptadaPorElPublico cancion = aceptacion cancion > 60

-- 5) Dado un instrumento y una canción, determinar si la canción necesita al instrumento para ser interpretada. 

loNecesita :: Instrumento -> Cancion -> Bool
loNecesita _ (UnaCancion _ _ []) = False    -- para cualquier instrumento si la cancion NO tiene instrumentos, tiro FALSE!! (Pattern Matching)
loNecesita instrumento cancion = elem instrumento (instrumentos cancion)

-- 6) Tocar una canción: esto implica que, si la canción es aceptada por el público, se la toca tal cual es, 
-- en caso contrario, se la toca con la duración reducida a la mitad.

tocarCancion :: Cancion -> Cancion
tocarCancion cancion 
    | aceptadaPorElPublico cancion = cancion
    | otherwise = cancion {duracion = duracion cancion / 2}