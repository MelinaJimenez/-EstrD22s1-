 module Queue (Queue,emptyQ, isEmptyQ, queue, firstQ, dequeue)
           where
	   
data Queue a = ConsQ [a]  deriving Show

--Una Queue es un tipo abstracto de datos de naturaleza FIFO (first in, first out). Esto significa
--que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el
--primero en salir (como la cola de un banco). Su interfaz es la siguiente:

emptyQ :: Queue a
--Crea una cola vacía.
emptyQ = ConsQ []

isEmptyQ :: Queue a -> Bool
--Dada una cola indica si la cola está vacía.
isEmptyQ  (ConsQ xs) = null xs

queue :: a -> Queue a -> Queue a
--Dados un elemento y una cola, agrega ese elemento a la cola.
queue e (ConsQ xs) =  ConsQ (agregarAQueue e xs)

agregarAQueue :: a -> [a] -> [a]
agregarAQueue e  [] =  [e]
agregarAQueue e xs  =  xs ++ [e]

firstQ :: Queue a -> a
--Dada una cola devuelve el primer elemento de la cola.
firstQ (ConsQ xs) = primerElemento xs

primerElemento:: [a] -> a
primerElemento   []   = error "la lista esta vacia"
primerElemento (x:xs) = x 

dequeue :: Queue a -> Queue a
--Dada una cola la devuelve sin su primer elemento.
dequeue (ConsQ xs) = ConsQ (sinPrimerElemento xs)

sinPrimerElemento:: [a]-> [a]
sinPrimerElemento   []  = []
sinPrimerElemento (x:xs)= xs