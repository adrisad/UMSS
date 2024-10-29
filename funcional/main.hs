-- 1. Implementa una función que cuente cuántos números pares hay en una lista
-- Ejemplo: contarPares [1,2,3,4,5,6] = 3
contarPares :: Integral x => [x] -> Int
contarPares = foldr (\x acc -> if even x then acc + 1 else acc) 0

-- Implementa una función usando foldr que concatene una lista de listas/strings
unir :: [[a]] -> [a]
unir = foldr (++) []

-- Implementa una función usando foldr que cuente los elementos de una lista
contarElementos :: [a] -> Int
contarElementos = foldr (\x acc -> acc+1) 0

-- Implementa usando foldr una función que cuente cuántos elementos cumplen un predicado (función similar a length . filter)
verificarElemento :: ( a -> Bool ) -> [a] -> Int
verificarElemento f = foldr (\x acc -> if f x then acc+1 else acc) 0

-- Implementa using foldr una función que elimine los números negativos de una lista
eliminarNegativos :: (Num a,Ord a) =>[a] -> [a] 
eliminarNegativos = foldr (\x acc -> if x < 0 then acc else [x]++acc) []
