-- ✅ 1 Definir una función que compare 2 listas y devuelva True si las listas son iguales
compararListas :: Eq a =>[a] -> [a] -> Bool
compararListas [] [] = False
compararListas [] _ = False
compararListas _ [] = False
compararListas (x:xs) (y:ys) = x == y && compararListas xs ys

-- ✅ 2 Definir una función que fusione 2 listas ordenadas en una 3ra. ordenada (sin necesidad de ordenar).
fusionarListas :: Ord a => [a] -> [a] -> [a]
fusionarListas [] xs = xs
fusionarListas ys [] = ys
fusionarListas (x:xs) (y:ys)
    | x <= y = x : fusionarListas xs (y:ys) 
    | otherwise = y : fusionarListas (x:xs) ys 

-- ✅ 3 Definir una función que verifique si una lista de listas podría ser considerada una matriz
esMatriz :: Eq a => [[a]] -> Bool
esMatriz [] = False
esMatriz [x] = True
esMatriz (x:y:xs)
    | xs == [] = length x == length y
    | otherwise = length x == length y && esMatriz (y:xs)

-- ✅ 4 Definir una función que reciba 1 matriz y una función de orden y devuelva True
--   si la matriz esta ordenada de acuerdo a la función de orden. 
matrizOrdenda :: [[a]] -> (a -> a -> Bool) -> Bool
matrizOrdenda [] _ = True
matrizOrdenda [x] f = listaOrdenada x f
matrizOrdenda (x:xs) f = listaOrdenada x f && matrizOrdenda xs f
listaOrdenada :: [a] -> (a -> a -> Bool) -> Bool
listaOrdenada [] _ = True
listaOrdenada [x] _ = True
listaOrdenada (x:y:xs) f = f x y && listaOrdenada (y:xs) f

-- ✅ 5 Definir una función que reciba una lista de números y devuelva todos los
--   números pares
losPares :: Integral a => [a] -> [a]
losPares [] = []
losPares (x:xs)
    | even x = x : losPares xs
    | otherwise = losPares xs

-- ✅ 6 Definir una función que reciba una lista de listas y devuelva solo aquellas cuya
--   longitud sea par.
tamanioPar :: Integral a => [[a]] -> [[a]]
tamanioPar [] = []
tamanioPar (x:xs)
    | (even.length) x = x : tamanioPar xs
    | otherwise = tamanioPar xs

-- ✅ 7 Definir una función que reciba una lista de listas de números y borre todos los
--   números pares de estas listas
eliminarParesLista :: Integral a => [[a]] -> [[a]]
eliminarParesLista [] = []
eliminarParesLista (x:xs) = listaNoPares x : eliminarParesLista xs
listaNoPares :: Integral a => [a] -> [a]
listaNoPares [] = []
listaNoPares (x:xs) = if even x then listaNoPares xs else x : listaNoPares xs

-- ✅ 8 Definir una función que reciba una lista de listas y devuelva una lista formada
--   por los penúltimos elementos de las listas
listaDePenultimos :: [[a]] -> [a]
listaDePenultimos [] = []
listaDePenultimos (x:xs)
    | length x > 1 = penultimo x : listaDePenultimos xs
    | otherwise = listaDePenultimos xs
penultimo :: [a] -> a
penultimo xs = xs !! (length xs - 2)

-- ✅ 9 Definir una función que reciba un número y devuelva una lista con los posibles
--   divisores del número.
divisores :: Integral a => a -> [a]
divisores 0 = []
divisores n = residuos n 1
residuos :: Integral a => a -> a -> [a]
residuos n c
    | n==c = [n]
    | mod n c == 0 = c : residuos n (c+1)
    | otherwise = residuos n (c+1)

-- ✅ 10 Definir una función que busque un elemento en una lista mediante búsqueda
--    secuencial (la función debería devolver la posición donde se encuentra el
--    elemento).
buscar :: Eq a => [a] -> a -> Int
buscar [] _ = -1
buscar (x:xs) e 
    | x==e = 0
    | res == -1 = -1
    | otherwise = 1 + res
    where
        res = buscar xs e

-- ❌ 11  Definir una función que busque un elemento en una lista mediante búsqueda
--    binaria (la función debería devolver la posición donde está el elemento).


-- ❌ 12. Definir una función que realice el ordenamiento de una lista de números por el
--     método de Selección Directa

-- ❌ 13. Definir una función que realice el ordenamiento de una lista de números por el
--     método de Inserción Directa

-- ❌ 14 Definir una función que realice el ordenamiento de una lista de números por el
--    método de Intercambio Directo o Burbuja

-- ❌ 15. Definir una función que realice el ordenamiento de una lista de números por el
--     método de QuickSort.

-- ✅ 16. Definir una función que reciba una matriz y devuelva su transpuesta
transpuesta :: [[a]] -> [[a]]
transpuesta [] = []
transpuesta matriz = (map head matriz) : transpuesta (map tail matriz)

-- ❌ 17. Definir una función que reciba 2 matrices y las multiplique

-- ❌ 18. Definir una función que reciba 3 matrices y las multiplique

-- ❌ 19. Definir una función que reciba 4 matrices y las multiplique

-- ❌ 20. Definir una función que reciba una lista de matrices y retorne su productoria

-- ❌ 21. Definir una función (f xs ys ) que verifique si la lista xs está incluida en la lista
--         ys, devolviendo verdadero o falso según caso.

-- ❌ 22. Definir una función (g xs ys ) que devuelva las posiciones de inicio donde xs
--         está en ys. Por ejemplo:
--         g [1,1,1] [3,1,1,1,1,4,1,1,3,1,1,1,7] => [1,2,9]





quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort mayores
    where
        menores = [y | y <- xs, y <= x]
        mayores = [y | y <- xs, y > x]