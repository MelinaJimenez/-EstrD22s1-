---------Priority Queue (cola de prioridad)
data PriorityQueue a= ConsPQ [a] deriving Show
--INVARIANTE: Recibe una lista ordenada de > a <.

--COSTO: O(1), devuelve una lista vacía.
--Propósito: devuelve una priority queue vacía.
emptyPQ :: PriorityQueue a
emptyPQ = ConsPQ[]

--COSTO: O(1),solo accedo una vez para saber si la lista es vacía.
--Propósito: indica si la priority queue está vacía.
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (ConsPQ xs) = null xs

--COSTO:O(1), utiliza la funcion agregarElemento que es constante.
--Propósito: inserta un elemento en la priority queue.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ e (ConsPQ xs) = ConsPQ (agregarElemento e xs)

--COSTO:O(1), el cons es constante ya que agrega una vez el elemento a la lista
agregarElemento::Ord a => a -> [a] -> [a]
agregarElemento e [] = [e]
agregarElemento e xs = e:xs

--COSTO:O(n), la funcion elMinimo es lineal.
--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (ConsPQ xs) =  elMinimo xs


--COSTO:O(n), recorre n veces la longitud de la lista.
elMinimo :: Ord a => [a] -> a
elMinimo   []   = error "esta vacia"
elMinimo (x:xs) =  min x (elMinimo xs)



--COSTO:O(n), la funcion sacarMin es lineal.
--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (ConsPQ xs) = ConsPQ (sacarMin (elMinimo xs) xs)

--COSTO:O(n), recorre n elementos de la longitud de la lista.
sacarMin :: Ord a => a -> [a] -> [a]
sacarMin e (x:xs) = if e == x
                     then xs
					  else x : sacarMin e xs
