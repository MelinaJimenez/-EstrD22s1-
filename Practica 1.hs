-----PRACTICA 1-----

--NÚMEROS ENTEROS

sucesor:: Int -> Int
sucesor n = n+1

sumar:: Int -> Int -> Int
sumar n m  = n+m

divisionYResto:: Int -> Int -> (Int,Int)
divisionYResto n m = (div n m, mod n m)

maxDelPar:: (Int,Int) -> Int
maxDelPar (x,y) = if x>y
					then x
					else y
					
--TIPOS ENUMERATIVOS

data Dir = Norte|Sur|Este|Oeste

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur   = Norte
opuesto Este  = Oeste
opuesto Oeste =	Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales	Sur Sur     = True		
iguales Este Este   = True
iguales Oeste Oeste = True
iguales  _    _     = False


siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
siguiente Oeste = Norte

--En el caso que no exista la siguien dirección a Oeste la función es
--PARCIAL. Precondición: No hay Siguiente de Oeste.


data DiaDeSemana = Lunes|Martes|Miercoles|Jueves|Viernes|Sabado|Domingo

primeroYUltimoDia :: (DiaDeSemana,DiaDeSemana)
primeroYUltimoDia = (Lunes,Domingo)


empiezaConM ::DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM   _       = False


vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = diaDeSemanaEnNumeros dia1 > diaDeSemanaEnNumeros dia2

diaDeSemanaEnNumeros :: DiaDeSemana -> Int
diaDeSemanaEnNumeros Lunes     = 1
diaDeSemanaEnNumeros Martes    = 2
diaDeSemanaEnNumeros Miercoles = 3
diaDeSemanaEnNumeros Jueves    = 4
diaDeSemanaEnNumeros Viernes   = 5
diaDeSemanaEnNumeros Sabado    = 6
diaDeSemanaEnNumeros Domingo   = 7


estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes   = False
estaEnElMedio Domingo = False
estaEnElMedio   _     = True

negar :: Bool -> Bool
negar True  = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica  _     _   = True

and :: Bool -> Bool -> Bool
and False _ = False
and _     b = b

or :: Bool -> Bool -> Bool
or True _ = True
or  _   b = b


--REGISTROS

data Persona = P String Int

nombre :: Persona -> String
nombre P n _ = n

edad : Persona -> Int
edad P _ e = e


crecer :: Persona -> Persona
crecer P n e = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre x (P n e) = P x e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2)
						then p1
						else p2
						
data TipoDePokemon = Agua|Fuego|Planta
data Pokemon       = P TipoDePokemon Int
data Entrenador    = E String Pokemon Pokemon


superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = supera (tipo p1) (tipo p2)

tipo :: Pokemon -> TipoDePokemon
tipo P t _  = t

supera :: TipoDePokemon -> TipoDePokemon -> Bool
supera Agua Fuego   = True
supera Fuego Planta = True
supera Planta Agua  = True
supera	_		_	= False


cantidadDePokemonDe:: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t (E n p1 p2) = sumaUnoSiEsIgual t (tipo p1) +
									sumaUnoSiEsIgual t (tipo p2)
									
sumaUnoSiEsIgual :: TipoDePokemon -> TipoDePokemon -> Int
sumaUnoSiEsIgual Agua Agua     = 1
sumaUnoSiEsIgual Fuego Fuego   = 1
sumaUnoSiEsIgual Planta Planta = 1
sumaUnoSiEsIgual   _      _    = 0


juntarPokemon :: (Entrenador,Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) }= devolverTodosLosPokemon e1 ++
						  devolverTodosLosPokemon e2
						  
						  
devolverTodosLosPokemon ::Entrenador -> [Pokemon]
devolverTodosLosPokemon E _ p1 p2 = p1 : p2 : []


--FUNCIONES POLIMORFICAS

loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete _ = 7

swap :: (a,b) -> (b,a)
swap (x,y)  = (y,x)


--PATTERN MATCHING

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  =False

elPrimero :: [a] -> a
elPrimero  []    = error "esta vacia"
elPrimero (x:xs) = x


sinElPrimero :: [a] -> [a]
sinElPrimero []     = []
sinElPrimero (x:xs) = xs


splitHead :: [a] -> (a,[a])
splitHead [] = error "esta vacia"
splitHead (x:xs) = (x,xs)