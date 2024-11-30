{-
Definir una función para:
1. Calcular la profundidad de un árbol
2. Calcular el total de hojas de un árbol
3. Obtener en una lista las hojas del árbol
4. Definir la función foldTree y utilizarla para:
   a) Calcular la profundidad
   b) Calcular el total de hojas
   c) Obtener la lista de las hojas del árbol
-}

-- ejercicio 1
data Arbol a = Hoja a | Rama (Arbol a) (Arbol a)
profundidadDeArbol :: Arbol a -> Int
profundidadDeArbol (Hoja _) = 1
profundidadDeArbol (Rama izq der) = 1 + max (profundidadDeArbol izq) (profundidadDeArbol der)

-- ejercicio 2
cantidadDeHojas :: Arbol a -> Int
cantidadDeHojas (Hoja _) = 1
cantidadDeHojas (Rama izq der) = cantidadDeHojas izq + cantidadDeHojas der

-- ejercicio 3
listaDeHojas :: Arbol a -> [a]
listaDeHojas (Hoja a) = [a]
listaDeHojas (Rama izq der) = listaDeHojas izq ++ listaDeHojas der

-- ejercicio 4

foldTree :: (b -> b -> b) -> (a -> b) -> Arbol a -> b
foldTree f g (Hoja a) = g a
foldTree f g (Rama izq der) = f (foldTree f g izq) (foldTree f g der)

-- a)
profundidad :: Arbol a -> Int
profundidad = foldTree (\izq der ->  1 + max izq der) (const 1)

-- b)
contadorDeHojas :: Arbol a -> Int
contadorDeHojas = foldTree (+) (const 1)

-- c)
listaHojas :: Arbol a -> [a]
listaHojas = foldTree (++) pure

