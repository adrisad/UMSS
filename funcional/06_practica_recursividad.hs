-- 1 Definir una función que compare 2 listas y devuelva True si las listas son iguales
compararListas :: Eq a =>[a] -> [a] -> Bool
compararListas [] [] = False
compararListas [] _ = False
compararListas _ [] = False
compararListas (x:xs) (y:ys) = x == y && compararListas xs ys

-- 2 Definir una función que fusione 2 listas ordenadas en una 3ra. ordenada (sin necesidad de ordenar).
fusionarListas :: Ord a => [a] -> [a] -> [a]
fusionarListas [] xs = xs
fusionarListas ys [] = ys
fusionarListas (x:xs) (y:ys)
    | x <= y = x : fusionarListas xs (y:ys) 
    | otherwise = y : fusionarListas (x:xs) ys 

-- 3 Definir una función que verifique si una lista de listas podría ser considerada una matriz
esMatriz :: Eq a => [[a]] -> Bool
esMatriz [] = False
esMatriz [x] = True
esMatriz (x:y:xs)
    | xs == [] = length x == length y
    | otherwise = length x == length y && esMatriz (y:xs)

-- 4 Definir una función que reciba 1 matriz y una función de orden y devuelva True
--   si la matriz esta ordenada de acuerdo a la función de orden. 
matrizOrdenda :: [[a]] -> (a -> a -> Bool) -> Bool
matrizOrdenda [] _ = True
matrizOrdenda [x] f = listaOrdenada x f
matrizOrdenda (x:xs) f = listaOrdenada x f && matrizOrdenda xs f
listaOrdenada :: [a] -> (a -> a -> Bool) -> Bool
listaOrdenada [] _ = True
listaOrdenada [x] _ = True
listaOrdenada (x:y:xs) f = f x y && listaOrdenada (y:xs) f

-- 5 Definir una función que reciba una lista de números y devuelva todos los
--   números pares
losPares :: Integral a => [a] -> [a]
losPares [] = []
losPares (x:xs)
    | even x = x : losPares xs
    | otherwise = losPares xs

-- 6 Definir una función que reciba una lista de listas y devuelva solo aquellas cuya
--   longitud sea par.
tamanioPar :: Integral a => [[a]] -> [[a]]
tamanioPar [] = []
tamanioPar (x:xs)
    | (even.length) x = x : tamanioPar xs
    | otherwise = tamanioPar xs

-- 7 Definir una función que reciba una lista de listas de números y borre todos los
--   números pares de estas listas
eliminarParesLista :: Integral a => [[a]] -> [[a]]
eliminarParesLista [] = []
eliminarParesLista (x:xs) = listaNoPares x : eliminarParesLista xs
listaNoPares :: Integral a => [a] -> [a]
listaNoPares [] = []
listaNoPares (x:xs) = if even x then listaNoPares xs else x : listaNoPares xs

-- 8 Definir una función que reciba una lista de listas y devuelva una lista formada
--   por los penúltimos elementos de las listas
listaDePenultimos :: [[a]] -> [a]
listaDePenultimos [] = []
listaDePenultimos (x:xs)
    | length x > 1 = penultimo x : listaDePenultimos xs
    | otherwise = listaDePenultimos xs
penultimo :: [a] -> a
penultimo xs = xs !! (length xs - 2)

-- 9 Definir una función que reciba un número y devuelva una lista con los posibles
--   divisores del número.
divisores :: (Eq a,Int a) => a -> [a]
divisores 0 = []
divisores n
    | residuo n n == 0 = divisores (n-1) : n
    | otherwise = divisores (n-1)

residuo :: Int -> Int -> Int
residuo n d
    | n<d = n
    | otherwise = residuo (n-d) d

-- 10 Definir una función que busque un elemento en una lista mediante búsqueda
--    secuencial (la función debería devolver la posición donde se encuentra el
--    elemento).

-- 11 Definir una función que busque un elemento en una lista mediante búsqueda
--    binaria (la función debería devolver la posición donde está el elemento).


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort mayores
    where
        menores = [y | y <- xs, y <= x]
        mayores = [y | y <- xs, y > x]