-- 1. Implementa una función que cuente cuántos números pares hay en una lista
-- Ejemplo: contarPares [1,2,3,4,5,6] = 3

contarPares :: Integral x => [x] -> Int
contarPares = foldr (\x acc -> if even x then acc + 1 else acc) 0

