sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatotia (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (x:xs) = sucesor x : sucesores xs


sucesor :: Int -> Int
sucesor x = x + 1

conjuncion :: [Bool] -> Bool
conjuncion []       = True
conjuncion (x : xs) = x && conjuncion xs

disyuncion :: [Bool] -> Bool
disyuncion []       = False
disyuncion (x : xs) = x || disyuncion xs

aplanar :: [[a]] -> [a]
aplanar []       = []
aplanar (x : xs) = agregar x (aplanar xs)

pertenece :: Eq a => a -> [a] -> Bool
pertenece e []     = False
pertenece e (x:xs) = x == e || pertenece e xs

apariciones :: Eq a => a -> [a] -> Int
apariciones e []     = 0
apariciones e (x:xs) = if e == x 

                        then 1 + apariciones e xs
                        else 0 + apariciones e xs

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []     = []
losMenoresA n (x:xs) = if x < n
                        then x : losMenoresA n xs
                        else losMenoresA n xs

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n []     = []
lasDeLongitudMayorA n (x:xs) = if (longitud x) > n
                                then x : lasDeLongitudMayorA n xs
                                else lasDeLongitudMayorA n xs

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e     = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

agregar :: [a] -> [a] -> [a]
agregar [] ys     = ys
agregar xs []     = xs
agregar (x:xs) ys = x : concatenar xs ys

reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = agregarAlFinal (reversa xs) x

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys         = ys
zipMaximos xs []         = xs
zipMaximos (x:xs) (y:ys) = max x y : zipMaximos xs ys
            

elMinimo :: Ord a => [a] -> a
elMinimo []       = error "no tiene elementos"
elMinimo [x]      = x
elMinimo (x:y:xs) =  min x (elMinimo xs)


------Recursión sobre números

factorial :: Int -> Int
factorial 0 = 1
factorial n= n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n >= 1
                     then n : cuentaRegresiva (n-1)
                    else []

repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir (n-1) e

losPrimeros :: Int -> [a] -> [a]
losPrimeros n (x:xs) = if n /= 0
                        then x : losPrimeros (n-1) xs
                        else []

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros n []     = []
sinLosPrimeros n (x:xs) = if n < 1
                           then sinLosPrimeros (n-1) xs
                           else xs
 
---------------------------Registros --------------

data Persona = P String Int deriving Show 

mayoresA:: Int -> [Persona]-> [Persona]
mayoresA _ []     = []
mayoresA n (x:xs) = if edad x > n
                     then x : mayoresA n  xs
                     else mayoresA n xs

edad :: Persona -> Int
edad (P _ e) =  e

promedioDeEdad:: [Persona] -> Int
promedioDeEdad ps = div (sumarEdad ps) (longitud ps)

sumarEdad:: [Persona] -> Int
sumarEdad []     = 0
sumarEdad (x:xs) = edad x + sumarEdad xs

elMasViejo :: [Persona] -> Persona
elMasViejo [p]    = p
elMasViejo (p:ps) = if edad p > (edad (elMasViejo ps))
                      then p 
                      else elMasViejo ps

data TipoDePokemon = Agua | Fuego| Planta deriving Show         
data Pokemon       = ConsPokemon TipoDePokemon Int deriving Show 
data Entrenador    = ConsEntrenador String [Pokemon] deriving Show 

cantPokemon:: Entrenador -> Int 
cantPokemon (ConsEntrenador _ pks) = longitud pks

cantPokemonDe:: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tipo (ConsEntrenador _ ps) = cantPokemones tipo ps

cantPokemones:: TipoDePokemon -> [Pokemon] -> Int
cantPokemones tipo (x:xs) = unoSi(sonIguales tipo (tipoDe x)) + cantPokemones tipo xs

unoSi:: Bool -> Int
unoSi True  = 1
unoSi False = 0

sonIguales:: TipoDePokemon -> TipoDePokemon -> Bool
sonIguales Agua Agua    = True
sonIguales Fuego Fuego  = True
sonIguales Planta Planta= True

tipoDe:: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon t n) = t

losQueLeganan:: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeganan t e1 (ConsEntrenador _ ys) = if (cantPokemonDe t e1) > 0
                                            then aCuantosLeGana t ys
                                            else 0


aCuantosLeGana:: TipoDePokemon -> [Pokemon] -> Int
aCuantosLeGana t []     = 0
aCuantosLeGana t (x:xs) = unoSi (superaA t (tipoDe x)) + aCuantosLeGana t xs


superaA :: TipoDePokemon -> TipoDePokemon -> Bool
superaA Agua Fuego   = True
superaA Fuego Planta = True
superaA Planta Agua  = True
superaA	_ _          = False

esMaestroPokemon:: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ ps) = tieneTodosLosTipos ps

tieneTodosLosTipos:: [Pokemon] -> Bool
tieneTodosLosTipos []  = True
tieneTodosLosTipos pks = perteneceAgua pks && perteneceFuego pks && pertenecePlanta pks

perteneceAgua:: [Pokemon] -> Bool
perteneceAgua  []    = False
perteneceAgua (x:xs) = esAgua (tipoDe x) || perteneceAgua xs

esAgua:: TipoDePokemon -> Bool
esAgua Agua = True
esAgua _    = False

perteneceFuego:: [Pokemon] -> Bool
perteneceFuego  []    = False
perteneceFuego (x:xs) = esFuego (tipoDe x) || perteneceAgua xs

esFuego:: TipoDePokemon -> Bool
esFuego Fuego = True
esFuego _     = False

pertenecePlanta:: [Pokemon] -> Bool
pertenecePlanta  []    = False
pertenecePlanta (x:xs) = esPlanta (tipoDe x) || perteneceAgua xs

esPlanta:: TipoDePokemon -> Bool
esPlanta Planta = True
esPlanta _      = False


