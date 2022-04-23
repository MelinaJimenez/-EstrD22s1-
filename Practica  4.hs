--------------Pizzas

data Pizza = Prepizza| Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa| Queso| Jamon| Aceitunas Int deriving Show


cantidadDeCapas :: Pizza -> Int
--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas Prepizza         = 0
cantidadDeCapas (Capa ing pizza) =  1 + cantidadDeCapas pizza

----------------------------------
armarPizza :: [Ingrediente] -> Pizza
--Dada una lista de ingredientes construye una pizza
armarPizza []          = Prepizza
armarPizza (ing: ings) = agregarIngrediente ing (armarPizza ings)

agregarIngrediente:: Ingrediente -> Pizza -> Pizza
agregarIngrediente ing Prepizza       = Capa ing Prepizza
agregarIngrediente ing (Capa i pizza) = Capa i (agregarIngrediente ing pizza)

-----------------------------------
sacarJamon :: Pizza -> Pizza
--Le saca los ingredientes que sean jamón a la pizza
sacarJamon Prepizza         = Prepizza
sacarJamon (Capa ing pizza) = if esJamon ing 
								then(sacarJamon pizza)
								else (Capa ing (sacarJamon pizza))
									 
esJamon:: Ingrediente ->  Bool
esJamon Jamon = True
esJamon _     = False	

---------------------------------
tieneSoloSalsaYQueso :: Pizza -> Bool
--Dice si una pizza tiene salsa y queso
tieneSoloSalsaYQueso Prepizza         = False
tieneSoloSalsaYQueso (Capa ing pizza) = (esSalsaOQueso ing) && (tieneSoloSalsaYQueso pizza)

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso  _    = False



----------------------------------
duplicarAceitunas :: Pizza -> Pizza
--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas Prepizza        = Prepizza
duplicarAceitunas (Capa ing pizza)= if esAceituna ing
									 then Capa(duplicarAceituna ing) (duplicarAceitunas pizza)
									 else (Capa ing(duplicarAceitunas pizza))
									 
duplicarAceituna ::	Ingrediente -> Ingrediente
duplicarAceituna  (Aceitunas n)  = Aceitunas (n*2) 

esAceituna :: Ingrediente -> Bool
esAceituna (Aceitunas _) = True
esAceituna  _            = False

-----------------
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.	
cantCapasPorPizza []    =[] --DUDO
cantCapasPorPizza (p:ps)=  (cantCapasDePizza p): cantCapasPorPizza ps		

cantCapasDePizza:: Pizza -> (Int, Pizza)
cantCapasDePizza Prepizza         = (0,Prepizza)
cantCapasDePizza (Capa ing pizza) = cantCapasDePizza pizza 			

-------------------Mapa de tesoros (con bifurcaciones)
--Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
--cada cofre tiene un objeto, que puede ser chatarra o un tesoro.
data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre| Bifurcacion Cofre Mapa Mapa	deriving Show	 

--(Fin (Cofre [Tesoro]))

--mapa2= (Bifurcacion(Cofre [Chatarra])(Fin (Cofre [Chatarra]))(Fin (Cofre [Tesoro])))

--direcciones = [Izq,Der]
--mapa1= (Fin (Cofre [Tesoro]))

hayTesoro :: Mapa -> Bool
--Indica si hay un tesoro en alguna parte del mapa.
hayTesoro (Fin cofre)               = tieneTesoro cofre
hayTesoro (Bifurcacion cofre m1 m2) = tieneTesoro cofre || hayTesoro m1 ||hayTesoro m2

tieneTesoro:: Cofre -> Bool
tieneTesoro (Cofre [] )      = False
tieneTesoro (Cofre (o: objs))= (esTesoro o) || tieneTesoro (Cofre objs)

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False


hayTesoroEn :: [Dir] -> Mapa -> Bool
--Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
--lista vacía de direcciones.
hayTesoroEn [] mapa                           =  hayTesoro mapa 
hayTesoroEn (d:ds) mapa = hayTesoroEn ds (avanzarEn d mapa)

avanzarEn:: Dir -> Mapa -> Mapa
avanzarEn Izq (Bifurcacion c izq der) =izq
avanzarEn Der (Bifurcacion c izq der) =der



---------------Nave EspaciaL
--modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
--dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
--es la siguiente:
--data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
--data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
--data Sector = S SectorId [Componente] [Tripulante] deriving Show
--type SectorId = String
--type Tripulante = String
--data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
--data Nave = N (Tree Sector)

--sectores :: Nave -> [SectorId]
--Propósito: Devuelve todos los sectores de la nave.
--sectores N (_ Sector) = 


--poderDePropulsion :: Nave -> Int
--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
--el poder de propulsión es el número que acompaña al constructor de motores.
--barriles :: Nave -> [Barril]
--Propósito: Devuelve todos los barriles de la nave.
--agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
--asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.
--sectoresAsignados :: Tripulante -> Nave -> [SectorId]
--Propósito: Devuelve los sectores en donde aparece un tripulante dado



---------------------------------Manada de lobos
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo| Explorador Nombre [Territorio] Lobo Lobo| Cria Nombre deriving Show 
data Manada = M Lobo deriving Show 

--Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean
--crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura
--que corresponda en cada caso:
--buenaCaza :: Manada -> Bool
--Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
-- elAlfa :: Manada -> (Nombre, Int)
--Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
--con su cantidad de presas. Nota: se considera que los exploradores y crías tienen cero presas
--cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
--cero presas





losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron territorio  (M lobo) = exploraronTerritorio territorio lobo

exploraronTerritorio::Territorio -> Lobo -> [Nombre]
exploraronTerritorio t (Cria nom)                    = []
exploraronTerritorio t (Cazador nom presas l1 l2 l3) = (exploraronTerritorio t l1) ++ (exploraronTerritorio t l2) ++ (exploraronTerritorio t l3)
exploraronTerritorio t (Explorador nom ts l1 l2)     = if (perteneceT t  ts)
														 then nom : (exploraronTerritorio t l1) ++ (exploraronTerritorio t l2)
														 else  exploraronTerritorio t l1 ++ exploraronTerritorio t l2
														 
perteneceT :: Territorio -> [Territorio] -> Bool
perteneceT territorio []     = False
perteneceT territorio (t:ts) = territorio == t	|| perteneceT territorio ts

--------
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
--Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio
--y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
--dicho territorio. Los territorios no deben repetirse
exploradoresPorTerritorio (manada) = exploradores (sinRepetidosT (territorios manada)) manada

exploradores ::[Territorio] -> Manada -> [(Territorio, [Nombre])]
exploradores []     manada = []
exploradores (t:ts) manada =   (t,(losQueExploraron t manada)) : exploradores ts manada

sinRepetidosT::	[Territorio] -> [Territorio]
sinRepetidosT [] = []
sinRepetidosT (t:ts) = agregarSiHaceFalta t (sinRepetidosT ts)



agregarSiHaceFalta :: Territorio -> [Territorio] -> [Territorio]
agregarSiHaceFalta t [] = []
agregarSiHaceFalta t ts = if perteneceT t ts
							then ts
							else t:ts										 

territorios :: Manada -> [Territorio]
territorios (M lobo) = territoriosDelLobo lobo

territoriosDelLobo:: Lobo -> [Territorio]
territoriosDelLobo (Cria _) = []
territoriosDelLobo (Cazador n _ l1 l2 l3)= territoriosDelLobo l1 ++ territoriosDelLobo l2 ++ territoriosDelLobo l3
territoriosDelLobo (Explorador _ ts l1 l2) = ts ++ territoriosDelLobo l1 ++ territoriosDelLobo l2

-----------
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador nom (M lobo) =  superioresC nom lobo

superioresC :: Nombre -> Lobo -> [Nombre]
superioresC nom (Cria _)               = []
superioresC nom (Cazador n _ l1 l2 l3) = if nom == n
                                           then devolverNombres l1 ++ devolverNombres l2 ++ devolverNombres l3
										   else superioresC nom l1 ++ superioresC nom l2 ++ superioresC nom l3
superioresC nom (Explorador n ts l1 l2)= superioresC nom l1 ++ superioresC nom l2

devolverNombres:: Lobo -> [Nombre]
devolverNombres (Cria nom)             = [nom]
devolverNombres (Cazador n _ l1 l2 l3) = n : devolverNombres l1 ++ devolverNombres l2 ++ devolverNombres l3 
devolverNombres (Explorador n ts l1 l2)= n : devolverNombres l1 ++ devolverNombres l2

-------------------------



