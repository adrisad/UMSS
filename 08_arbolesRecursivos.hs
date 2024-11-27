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

data Arbol a = Hoja a | Rama (Arbol a) (Arbol a)
profundidadDeArbol :: Arbol a -> Int
profundidadDeArbol (Hoja _) = 1
profundidadDeArbol (Rama izq der) = 1 + max (profundidadDeArbol izq) (profundidadDeArbol der)