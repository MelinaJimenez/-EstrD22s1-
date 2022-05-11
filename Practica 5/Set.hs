--- Set (conjunto)
--Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:


module Set (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) 
       where
	   
data Set a = ConsS [a] Int deriving Show	   

--INVARIANTE: La lista no tiene repetidos

emptyS :: Set a
--Crea un conjunto vacío.
emptyS = ConsS [] 0


addS :: Eq a => a -> Set a -> Set a
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS e (ConsS xs n ) = ConsS (agregarElemento e xs) (unoSi (not (pertenece e xs))+ n)

agregarElemento:: Eq a => a -> [a] ->[a]
agregarElemento e [] = [e]
agregarElemento e xs = if pertenece e xs
                          then xs  
						   else (e:xs)
						   
unoSi:: Bool -> Int
unoSi True = 1
unoSi False = 0

pertenece :: Eq a => a -> [a] -> Bool
pertenece e []     = False
pertenece e (x:xs) = x == e || pertenece e xs						   

belongs :: Eq a => a -> Set a -> Bool
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs e (ConsS xs n) = pertenece e xs


sizeS :: Eq a => Set a -> Int
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (ConsS xs n) = n 

removeS :: Eq a => a -> Set a -> Set a
--Borra un elemento del conjunto.
removeS e (ConsS xs n) = ConsS (sacarElemento e xs)(n - (unoSi(pertenece e xs)))

sacarElemento :: Eq a => a -> [a] -> [a]
sacarElemento e  []    = []
sacarElemento e (x:xs) = if sonIguales e x
                           then xs 
						   else x:xs
						   
sonIguales :: Eq a => a -> a -> Bool
sonIguales x y = x == y

unionS :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS  (ConsS xs n) set2 = unirConjuntos xs set2

unirConjuntos:: Eq a => [a] -> Set a -> Set a
unirConjuntos  []        set      = set
unirConjuntos (x:xs) (ConsS ls n) = unirConjuntos xs (ConsS (agregarElemento x ls) (unoSi(not(pertenece x ls)) + n))

setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (ConsS xs _) = xs 