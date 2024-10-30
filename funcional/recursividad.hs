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

