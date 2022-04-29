--TIPOS RECURSIVOS SIMPLES

data Color = Azul| Rojo deriving Show 
data Celda = Bolita Color Celda | CeldaVacia deriving Show

nroBolitas::Color -> Celda -> Int
nroBolitas c CeldaVacia      = 0
nroBolitas c (Bolita col cel)= unoSi(esBolitaColor c col) + nroBolitas c cel

unoSi ::Bool -> Int
unoSi True  = 1
unoSi False = 0

esBolitaColor::Color -> Color -> Bool
esBolitaColor Azul Azul = True
esBolitaColor Rojo Rojo = True
esBolitaColor _ _       = False

--------------------------
poner::Color -> Celda -> Celda
poner c  cel= Bolita c cel

---------------------------
sacar::Color -> Celda -> Celda
sacar c CeldaVacia       = CeldaVacia
sacar c (Bolita col cel) = if(esBolitaColor c col)
		           then cel	
			   else Bolita col (sacar c cel)
							 
							 
-----------------------------
ponerN:: Int -> Color -> Celda -> Celda
ponerN 0 col celda = celda
ponerN n col celda = poner col(ponerN(n-1) col celda)


------------Camino hacia el tesoro
data Objeto = Cacharro | Tesoro deriving Show 
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show 

hayTesoro :: Camino -> Bool
hayTesoro Fin                 = False
hayTesoro (Nada camino)       = hayTesoro camino
hayTesoro (Cofre objs camino) = contieneTesoro objs || hayTesoro camino

contieneTesoro :: [Objeto] -> Bool
contieneTesoro    []    = False
contieneTesoro (o:objs) = esTesoro o || contieneTesoro objs

esTesoro:: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

---------------
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro  Fin              = 0
pasosHastaTesoro  (Nada camino)    = 1 + pasosHastaTesoro camino
pasosHastaTesoro(Cofre obs camino) =  totalDePasos obs (pasosHastaTesoro camino)


totalDePasos:: [Objeto] -> Int -> Int
totalDePasos obs n = if contieneTesoro obs
                      then 0
                      else 1 + n
											
----------
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n    Fin              = False
hayTesoroEn n (Nada camino)       = hayTesoroEn (n-1) camino
hayTesoroEn n (Cofre objs camino) = if n ==0
                                      then contieneTesoro objs
			     	      else hayTesoroEn (n-1) camino

---------------------
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n   Fin              = n == 0
alMenosNTesoros n (Nada camino)      = alMenosNTesoros n camino
alMenosNTesoros n (Cofre obs camino) = (cantTesoro obs) >= n || (alMenosNTesoros (n - cantTesoro obs) camino)

cantTesoro :: [Objeto] -> Int
cantTesoro    []    = 0
cantTesoro (o: objs)= unoSi(esTesoro o) +(cantTesoro objs)


cantTesorosEntre :: Int -> Int -> Camino -> Int
--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
--el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
--incluidos tanto 3 como 5 en el resultado.
cantTesorosEntre n1 n2 Fin                = 0
cantTesorosEntre n1 n2 (Nada camino)      = if n1 == 0 && n2 == 0
                                              then 0
                                               else cantTesorosEntre (darPaso n1) (darPaso n2) camino
cantTesorosEntre n1 n2 (Cofre obs camino) = if n1 == 0 && n2 == 0
                                             then cantTesoro obs
                                              else cantTesorosEntre (darPaso n1) (darPaso n2) camino

darPaso :: Int -> Int
darPaso 0 = 0
darPaso n = n-1


----------------- Árboles binarios
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show 

sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT x t1 t2) =  x + (sumarT t1) + (sumarT t2)

sizeT :: Tree a -> Int
sizeT   EmptyT       = 0
sizeT (NodeT x t1 t2)= 1 + (sizeT t1) + (sizeT t2)

mapDobleT :: Tree Int -> Tree Int
mapDobleT  EmptyT        = EmptyT
mapDobleT (NodeT x t1 t2)= NodeT (x*2) (mapDobleT t1) (mapDobleT t2)

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x   EmptyT       = False
perteneceT x (NodeT y t1 t2)= (x==y) || (perteneceT y t1)|| (perteneceT y t2)

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x    EmptyT       = 0
aparicionesT x (NodeT y t1 t2) = if (x==y)
				  then 1 + (aparicionesT y t1) + (aparicionesT y t2)
				  else (aparicionesT y t1) + (aparicionesT y t2)
									
leaves :: Tree a -> [a]
leaves    EmptyT               = []								
leaves (NodeT x t1 t2)         = if esEmpty t1 && esEmpty t2
                                  then [x]
                                  else leaves t1 ++ leaves t2
								  
esEmpty:: Tree a -> Bool
esEmpty EmptyT = True
esEmpty   _    = False

heightT :: Tree a -> Int
heightT  EmptyT         = 0
heightT (NodeT x t1 t2) = 1 + (max(heightT t1)(heightT t2))

mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

toList :: Tree a -> [a]
toList  EmptyT        = []
toList (NodeT x t1 t2)= (toList t1) ++ [x] ++ (toList t2)

levelN :: Int -> Tree a -> [a] 
levelN _ EmptyT          = []
levelN 0 (NodeT x _ _)   = [x]
levelN n (NodeT _ t1 t2) = levelN(n-1) t1 ++ levelN(n-1) t2

listPerLevel :: Tree a -> [[a]]
listPerLevel    EmptyT       = []
listPerLevel (NodeT x t1 t2) =[x]: (juntarListas(listPerLevel t1) (listPerLevel t2))

juntarListas ::[[a]] -> [[a]] -> [[a]]
juntarListas    xss      []    = xss
juntarListas    []      yss    = yss
juntarListas (xs:xss) (ys:yss) = (xs ++ ys) :  juntarListas xss yss

ramaMasLarga :: Tree a -> [a]
ramaMasLarga  EmptyT         = []
ramaMasLarga (NodeT x t1 t2) =   x: elegirRama (ramaMasLarga t1) (ramaMasLarga t2)
                                
elegirRama:: [a] -> [a]-> [a]
elegirRama xs ys =  if longitud xs > longitud ys
                       then xs
					   else ys

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos   EmptyT       = []
todosLosCaminos (NodeT x t1 t2)= agregarACadaSublista x ((todosLosCaminos t1) ++ (todosLosCaminos t2))


agregarACadaSublista:: a -> [[a]] -> [[a]]
agregarACadaSublista y   []     = [[y]]
agregarACadaSublista y (xs:xss) = (y:xs): agregarACadaSublista y xss

-----------------Expresiones Aritméticas

data ExpA = Valor Int| Sum ExpA ExpA| Prod ExpA ExpA| Neg ExpA deriving Show 
		  
eval :: ExpA -> Int
eval (Valor n)    = n
eval (Sum e1 e2)  = (eval e1) + (eval e2)
eval (Prod e1 e2) = (eval e1) * (eval e2)
eval (Neg e1 )    =  -(eval e1)

simplificar :: ExpA -> ExpA
simplificar (Valor n)   = Valor n
simplificar (Sum e1 e2) = armarSuma (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2)= armarProd (simplificar e1) (simplificar e2)
simplificar (Neg e1 )   = armarNegacion (simplificar e1)

armarSuma:: ExpA -> ExpA -> ExpA
armarSuma (Valor 0) e2 = e2
armarSuma e1 (Valor 0) = e1
armarSuma e1 e2        = Sum e1 e2

armarProd :: ExpA -> ExpA -> ExpA
armarProd (Valor 0) e2 = (Valor 0)
armarProd e1 (Valor 0) = (Valor 0)
armarProd (Valor 1) e2 = e2
armarProd e1 (Valor 1) = e1
armarProd e1 e2        = Prod e1 e2

armarNegacion :: ExpA -> ExpA
armarNegacion (Neg exp) = exp
armarNegacion  exp      = (Neg exp)

