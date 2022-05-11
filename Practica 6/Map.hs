module Map
    (Map, emptyM, assocM, lookupM, deleteM, keys) 
  where

data Map k v = M [(k,v)]
  {- INV.REP.: en M kvs, no hay claves repetidas en kvs -}

--COSTO:O(1), devuelve una lista de clave-valor
--Propósito: devuelve un map vacío
emptyM :: Map k v
emptyM = M [] 

--COSTO:O(1), la funcion  asociar es constante
--Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM   k v (M kvs) = M (asociar k v kvs)

--COSTO:O(1), porque agrega un elemento a una lista.
asociar::  Eq k => k -> v -> [(k,v)] -> [(k,v)]
asociar k v   []    = [(k,v)] --COSTO:O(1),cons es constante
asociar k v ((k1,v1):kvs) = if k == k1	
                              then (k1,v) : kvs --COSTO:O(1),cons es constante
							  else (k1,v1): asociar k v kvs --COSTO:O(1),cons es constante
							  

--COSTO:O(n), la funcion buscar es lineal 
--Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M kvs) = buscar k kvs 

--COSTO:O(n), es lineal 
buscar:: Eq k => k -> [(k,v)] -> Maybe v
buscar k   []         = Nothing
buscar k ((k1,v):kvs) = if k == k1	
                            then Just v --COSTO:O(1), devuelvo el elemento
							 else  buscar k kvs --COSTO:O(n), recorro n veces la lista
							

--COSTO: O(n), la funcion borrar es lineal
--Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M kvs) =  M(borrar k kvs)

--COSTO:O(n), es lineal ya que n es la longitud de la lista
borrar::Eq k => k -> [(k,v)] -> [(k,v)]
borrar k   []         = []
borrar k ((k1,v):kvs) = if k == k1
                          then  kvs
                           else  (k1,v): borrar k kvs ----COSTO:O(n),recorrer n veces los elementos de la lista
		--COSTO:O(1),cons es constante			  

--COSTO:O(1), la funcion clavesMap es constante
--Propósito: devuelve las claves del map
keys :: Map k v -> [k]
keys (M kvs) = clavesMap kvs

--COSTO:O(1),cons es constante
clavesMap::[(k,v)] -> [k]
clavesMap    []      = []
clavesMap ((k,v):kvs)= k : clavesMap kvs  
