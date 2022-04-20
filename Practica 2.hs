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
aplanar (xs : xss) = agregar xs (aplanar xss)

pertenece :: Eq a => a -> [a] -> Bool
pertenece e []     = False
pertenece e (x:xs) = x == e || pertenece e xs

apariciones :: Eq a => a -> [a] -> Int
apariciones e []     = 0
apariciones e (x:xs) = if e == x 
                        then 1 + apariciones e xs
                        else apariciones e xs

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
agregar (x:xs) ys = x : agregar xs ys

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
losPrimeros _ [] = []
losPrimeros 0 xs =[]
losPrimeros n (x : xs) = x : losPrimeros (n-1) xs


sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ [] = []
sinLosPrimeros 0 xs = xs
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs
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
cantPokemonDe tipo (ConsEntrenador _ ps) =  cantPokemonDeMismoTipo tipo ps

cantPokemonDeMismoTipo:: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDeMismoTipo tipo []     =  0
cantPokemonDeMismoTipo tipo (p:ps) = unoSi(mismoTipo tipo (tipoDe p)) + cantPokemonDeMismoTipo tipo ps

unoSi:: Bool -> Int
unoSi True  = 1
unoSi False = 0

mismoTipo:: TipoDePokemon -> TipoDePokemon -> Bool
mismoTipo Agua Agua    = True
mismoTipo Fuego Fuego  = True
mismoTipo Planta Planta= True
mismoTipo   _       _   = False

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
esMaestroPokemon (ConsEntrenador _ ps) = perteneceA ps Agua && perteneceA ps Fuego && perteneceA ps Planta

perteneceA:: [Pokemon] -> TipoDePokemon -> Bool
perteneceA  [] tipo= False
perteneceA (x:xs) tipo = mismoTipo tipo (tipoDe x) || perteneceA xs tipo




data Seniority = Junior | SemiSenior| Senior deriving Show
data Proyecto  = ConsProyecto String deriving Show
data Rol       = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa   = ConsEmpresa [Rol] deriving Show

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = sinProyectosRepetidos(listaDeProyectos rs)

listaDeProyectos :: [Rol] -> [Proyecto]
listaDeProyectos []     = []
listaDeProyectos (r:rs) = (proyectoDelRol r) : listaDeProyectos rs

proyectoDelRol :: Rol -> Proyecto
proyectoDelRol (Developer _ p)  = p
proyectoDelRol (Management _ p) = p

sinProyectosRepetidos:: [Proyecto] -> [Proyecto]
sinProyectosRepetidos []     = []
sinProyectosRepetidos (p:ps) = agregarSiHaceFalta p (sinProyectosRepetidos ps)

agregarSiHaceFalta :: Proyecto -> [Proyecto]-> [Proyecto]
agregarSiHaceFalta p ps = if perteneceP p ps		
							then ps
							else p:ps

perteneceP :: Proyecto -> [Proyecto] -> Bool
perteneceP p []     = False
perteneceP p (x:xs) = sonIguales x p || perteneceP p xs

sonIguales:: Proyecto -> Proyecto -> Bool
sonIguales (ConsProyecto p1) (ConsProyecto p2) = p1==p2

-------------------------
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior empresa ps= losQueTrabajanCon (totalDev empresa) ps

totalDev :: Empresa ->  [Rol]
totalDev (ConsEmpresa rs)= todosLosDev rs

todosLosDev :: [Rol] -> [Rol]
todosLosDev [] = []
todosLosDev (d:ds) = if esDev d && esSenior d
                      then d : todosLosDev ds
                      else todosLosDev ds

esDev :: Rol -> Bool
esDev (Developer s p) = True
esDev _               = False
 
esSenior :: Rol -> Bool
esSenior (Developer Senior _)= True
esSenior (Developer _ _)     = False
esSenior _                   = False

losQueTrabajanCon:: [Rol] -> [Proyecto] -> Int
losQueTrabajanCon rs [] = 0
losQueTrabajanCon rs (p:ps) = unoSi (perteneceARol p rs) + losQueTrabajanCon rs ps

perteneceARol :: Proyecto -> [Rol] -> Bool
perteneceARol p [] = False
perteneceARol p (x:xs) = sonIguales (proyecto x) p || perteneceARol p xs


proyecto :: Rol -> Proyecto
proyecto (Developer _ p) = p
proyecto (Management _ p) =  p

-----------
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (ConsEmpresa rs) =  losQueTrabajanCon rs ps

----------
asignadosPorProyecto :: Empresa -> [(Proyecto,Int)]
asignadosPorProyecto (ConsEmpresa roles) = tuplaSinRepetidos (asignadosConProyecto roles)

tuplaSinRepetidos :: [(Proyecto,Int)] -> [(Proyecto,Int)]
tuplaSinRepetidos [] = []
tuplaSinRepetidos (x:xs)= agregarSoloSiHaceFalta x (tuplaSinRepetidos xs)

agregarSoloSiHaceFalta :: (Proyecto,Int) -> [(Proyecto,Int)] -> [(Proyecto,Int)]
agregarSoloSiHaceFalta x [] = []
agregarSoloSiHaceFalta x xs = if perteneceT x xs
                               then xs
                               else x:xs

perteneceT :: (Proyecto,Int) -> [(Proyecto,Int)] -> Bool
perteneceT p []     = False
perteneceT p (x:xs) = sonIguales (primerElem p) (primerElem x) || perteneceT p xs

primerElem:: (Proyecto,Int) -> Proyecto
primerElem (p,n) = p

asignadosConProyecto :: [Rol] -> [(Proyecto,Int)]
asignadosConProyecto []     = []
asignadosConProyecto (x:xs) = crearTupla (proyecto x) xs : asignadosConProyecto xs

crearTupla :: Proyecto -> [Rol] -> (Proyecto, Int)
crearTupla p []    = error "es una lista vacia"
crearTupla p roles = (p, (cantP p roles))

cantP :: Proyecto -> [Rol] -> Int
cantP p []     = 0
cantP p (x:xs) = unoSi (sonIguales (proyecto x) p) + cantP p xs
