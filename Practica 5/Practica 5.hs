import Set
import Queue

data Tree a = EmptyT | NodeT  a (Tree a) (Tree a) deriving Show 

--Cálculo de costos

--Costo: O(1)
head :: [a] -> a
head (x:xs) = x --constante

--Costo: O(1)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 --constante

--Costo: O(n)
factorial :: Int -> Int
factorial 0 = 1 
factorial n = n * factorial (n-1)
                    --lineal

--Costo: O(n)
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
                       --lineal

--Costo: O(n2)
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs
                       --lineal     --cuadratica

--Costo: O(n)
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs
                  --constante     --lineal

--Costo: O(n2)
sinRepetidos1 :: Eq a => [a] -> [a]
sinRepetidos1 []    = []       --lineal
sinRepetidos1 (x:xs) = if pertenece x xs
                         then sinRepetidos1 xs  --cuadratica
                         else x : sinRepetidos1 xs
                                

--Costo: O(n)
-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys
                     --lineal

--Costo: O(n)
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs
                     --lineal

--Costo: O(n)
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs
                     --lineal

--Costo: O(n)
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs
                  --lineal

--Costo: O(n2)
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)
                --lineal   --lineal  = --cuadratica


--Costo: O(n)
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)
                  --lineal

--Costo: O(n)
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x --constante
                  then xs
                  else x : sacar n xs --lineal
				  
--Costo: O(n2)				  
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs = let m = minimo xs --lineal
              in m : ordenar (sacar m xs)  -- = cuadratica
			                   --lineal

---Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList
---- Como usuario del tipo abstracto Set implementar			  
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
--al conjunto.
losQuePertenecen   []    set = []
losQuePertenecen (x:xs)  set = if belongs x set
                                 then x:losQuePertenecen xs set
								 else losQuePertenecen xs set

sinRepetidos :: Eq a => [a] -> [a]
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos xs = setToList (agregarElementos xs)

agregarElementos :: Eq a => [a] -> Set a
agregarElementos   []   = emptyS
agregarElementos (x:xs) = addS x (agregarElementos xs)

unirTodos :: Eq a => Tree (Set a) -> Set a
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjunto del arbol.
unirTodos      EmptyT       = emptyS
unirTodos (NodeT set t1 t2) = unionS set(unionS(unirTodos t1)(unirTodos t2))

----------------------------------------------------------------
			  
-----Como usuario del tipo abstracto Queue implementar
lengthQ :: Queue a -> Int
--Cuenta la cantidad de elementos de la cola.
lengthQ  q =  if isEmptyQ q
               then 0  
			   else 1 + lengthQ(dequeue q)
		   

queueToList :: Queue a -> [a]
--Dada una cola devuelve la lista con los mismos elementos,
--donde el orden de la lista es el de la cola.
--Nota: chequear que los elementos queden en el orden correcto.
queueToList q = if isEmptyQ q
                  then []
			      else (firstQ q) : (queueToList (dequeue q))

unionQ :: Queue a -> Queue a -> Queue a
--Inserta todos los elementos de la segunda cola en la primera.	
unionQ 	 q1 q2 = if isEmptyQ q2
                   then q1
					else queue(firstQ q2)(unionQ q1(dequeue q2))
