-- Definir los siguientes tipos de datos

-- ZonaGeografica que permita representar las 3 zonas geográficas de Bolivia
-- (valles, llanos y altiplano).

data ZonaGeografica = Valles | Llanos | Altiplano
  deriving (Show,Eq)

-- Departamento que permita representar los 9 departamentos de Bolivia
data Departamento = 
    Pando 
  | Beni 
  | SantaCruz 
  | Cochabamba 
  | Chuquisaca 
  | Tarija 
  | Potosi 
  | Oruro 
  | LaPaz
  deriving (Show)

-- Utilizando estos tipos definir las siguientes funciones:

-- 1. Una función que reciba una zona y devuelva un mensaje indicando sus características.

caracteristica :: Departamento -> String
caracteristica Pando       = "Pando es una región cálida y está ubicada en los llanos amazónicos."
caracteristica Beni        = "Beni es conocido por sus extensas llanuras y biodiversidad."
caracteristica SantaCruz   = "Santa Cruz es el motor económico del país, ubicado en los llanos."
caracteristica Cochabamba  = "Cochabamba es famoso por su clima templado y está en los valles."
caracteristica Chuquisaca  = "Chuquisaca es la cuna de la independencia y está en los valles."
caracteristica Tarija      = "Tarija es conocida por su producción de vino y su ubicación en los valles."
caracteristica Potosi      = "Potosí es una región minera famosa por el Cerro Rico y está en el altiplano."
caracteristica Oruro       = "Oruro es conocida por su carnaval y está en el altiplano."
caracteristica LaPaz       = "La Paz, sede de gobierno, se encuentra en el altiplano."

-- 2. Una función que reciba un departamento y devuelva True si pertenece a la zona de los valles, falso en otro caso.

esDeLosValles :: Departamento -> Bool
esDeLosValles Cochabamba = True
esDeLosValles Chuquisaca = True
esDeLosValles Tarija     = True
esDeLosValles _          = False

-- 3. Una función que reciba un departamento y devuelva la zona a la que corresponde el departamento.

zonaDepartamento :: Departamento -> ZonaGeografica
zonaDepartamento Pando       = Llanos
zonaDepartamento Beni        = Llanos
zonaDepartamento SantaCruz   = Llanos
zonaDepartamento Cochabamba  = Valles
zonaDepartamento Chuquisaca  = Valles
zonaDepartamento Tarija      = Valles
zonaDepartamento Potosi      = Altiplano
zonaDepartamento Oruro       = Altiplano
zonaDepartamento LaPaz       = Altiplano

-- 4. Una función que reciba una lista de departamentos y devuelva aquellos que pertenecen a la zona de los llanos o de los valles.

sonDeLaZona :: Departamento -> Bool
sonDeLaZona dep
  | zonaDepartamento dep == Llanos = True
  | zonaDepartamento dep == Valles = True
  | otherwise = False

cualesSonDeLaZona :: [Departamento] -> [Departamento]
cualesSonDeLaZona = filter sonDeLaZona


-- II. Sean las siguientes definiciones de tipo:
type Dia = Int
type Mes = Int
type Anio = Int
type Fecha = (Dia, Mes, Anio)
type Periodo = (Fecha, Fecha)
type Nombre = String
type Presidente = (Nombre, Periodo)
-- El tipo Presidente es un par que representa el Nombre del presidente y el período de tiempo en que gobernó.

-- Definir:
-- 1. Una función que reciba un Periodo y devuelva el tiempo transcurrido en años.

tiempoEnAnios :: Periodo -> Int
tiempoEnAnios ((_, _, anioInicio), (_, _, anioFin)) = anioFin - anioInicio

-- 2. Una función que reciba un Presidente y devuelva el tiempo total en años que gobernó.

governo :: Presidente -> Int
governo (_, p) = tiempoEnAnios p

-- 3. Definir una función que reciba dos presidentes y devuelva aquel que gobernó más tiempo.

mayorGovierno :: Presidente -> Presidente -> Presidente
mayorGovierno p1 p2
  | governo p1 > governo p2 = p1
  | otherwise = p2

-- 4. Una función que reciba una lista de presidentes y devuelva el nombre del presidente que menos tiempo gobernó.

menorGovierno :: [Presidente] -> Nombre
menorGovierno [] = "no hay presidentes"
menorGovierno [(nombre,_)] = nombre
menorGovierno (x:y:xs)
  | governo x < governo y = menorGovierno (x:xs)
  | otherwise = menorGovierno (y:xs)

-- 5. Una función que reciba una lista de presidentes y devuelva una lista con los nombres de los presidentes que gobernaron antes del año 1990.

esDeAntesDel1990 ::  Periodo -> Bool
esDeAntesDel1990 ((_,_,inicio),_) = inicio < 1990

governoAntesDel1990 :: [Presidente] -> [Nombre]
governoAntesDel1990 presidentes = [nombre|(nombre,periodo)<-presidentes,esDeAntesDel1990 periodo]

-- 6. Una función que reciba una lista de presidentes y devuelva la cantidad de presidentes que gobernaron menos de 4 años.

goviernosMasDe4Anios :: [Presidente] -> Int
goviernosMasDe4Anios presidentes = length [p | p <- presidentes, governo p < 4]

-- 7. Una función que reciba una lista de presidentes y la ordene ascendentemente por la fecha en que fue presidente.

insertarOrdenando :: Presidente -> [Presidente] -> [Presidente]
insertarOrdenando p [] = [p]
insertarOrdenando p (x:xs)
  | compararFechas (fechaInicio p) (fechaInicio x) = p:x:xs
  | otherwise = x : insertarOrdenando p xs

compararFechas :: Fecha -> Fecha -> Bool
compararFechas (d1,m1,a1) (d2,m2,a2)
  | a1 < a2 = True
  | m1 < m2 && a1 == a2 = True
  | d1 < d2 && m1 == m2 && a1 == a2 = True
  | otherwise = False

fechaInicio :: Presidente -> Fecha
fechaInicio (_,(inicio,_)) = inicio

ordenarPresidentes :: [Presidente] -> [Presidente]
ordenarPresidentes [] = []
ordenarPresidentes (x:xs) = insertarOrdenando x (ordenarPresidentes xs)