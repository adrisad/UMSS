-- Definición básica de potencia recursiva
-- x elevado a la n
potenciaRecursiva :: Int -> Int -> Int
potenciaRecursiva x n
    | n==0  = 1
    | otherwise = x * potenciaRecursiva x (n-1)

-- Definición básica de suma recursiva
sumaRecursiva :: Int -> (Ord b, Num b) => b -> Int
sumaRecursiva a b 
    | b==0 = a
    | b>0 = sumaRecursiva (a+1) (b-1)

-- Definir la multiplicacion recursiva
multiplicacionRecursiva :: (Ord a, Num a) => a -> a -> a
multiplicacionRecursiva a b
    | a==0 = 0
    | a>0 = b + multiplicacionRecursiva (a-1) b

-- Definir la factorial recursiva
factorial :: (Num b, Ord b) => b -> b
factorial n 
    | n==0 = 1
    | n>0 = n * factorial (n-1)

-- Definir la funcion lenght recursiva
myLenght :: [a] -> Int
myLenght [] = 0
myLenght (a:ax) = 1 + myLenght (ax)

-- Cociente de la división entera de dos números naturales
cociente :: Int -> Int -> Int
cociente n d
    | n<d = 0
    | otherwise = 1 + cociente (n-d) d

-- Residuo de la división entera de dos números naturales.
residuo :: Int -> Int -> Int
residuo n d
    | n<d = n
    | otherwise = residuo (n-d) d

-- Resta de listas (\\)
restaLista :: Eq a => [a] -> [a] -> [a]
restaLista [] _ = []
restaLista (x:xs) l2
    | elem x l2 = restaLista xs l2
    | otherwise = x : restaLista xs l2

-- Una función que reciba una lista y una función de
-- orden y retorne True si la lista está ordenada de
-- acuerdo a la función, falso en otro caso.
listaOrdenda :: (a -> a -> Bool) -> [a] -> Bool
listaOrdenda _ [] = True
listaOrdenda _ [x] = True
listaOrdenda f (x:y:xs) = f x y && listaOrdenda f (y:xs)

-- Una función que reciba una lista xs y un número n y
-- rote los elementos de la lista n veces a la derecha.
rotarLista :: Int -> [a] -> [a]
rotarLista _ [] = []
rotarLista 0 xs = xs
rotarLista n (x:xs) = rotarLista (n-1) (xs++[x])

