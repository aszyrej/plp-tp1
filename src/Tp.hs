module Tp where

import Data.List

type Texto = String
type Feature = Float
type Instancia = [Feature]
type Extractor = (Texto -> Feature)

type Datos = [Instancia]
type Etiqueta = String
type Modelo = (Instancia -> Etiqueta)
type Medida = (Instancia -> Instancia -> Float)

tryClassifier :: [Texto] -> [Etiqueta] -> Float
tryClassifier x y = let xs = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x in
    nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

-- Ej1
-- Dado un elemento separador y una lista, parte la lista en sublistas de acuerdo a la 
-- aparicion del separador. Eliminando en el proceso las apariciones del separador.
--
split :: Eq a => a -> [a] -> [[a]]
split e = filter (/= []) . foldr 
                    (\x rec -> if x == e then []:rec else (x:(head rec)):(tail rec))
                    [[]]

-- Ej2 - Precond: texto no vacío
-- Dado un texto, calcula la longitud promedio de sus palabras. 
-- Consideraremos palabra a cualquier secuencia de caracteres separadas por espacios.
--
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras txt = let palabras = split ' ' txt
                                   longitudes = map genericLength palabras
                               in  mean longitudes

-- Ej 3
-- Dada una lista, devuelve la cantidad de veces que aparece cada elemento en la misma.
-- Se utiliza nub para obtener la lista sin repeticiones y luego mapear su frecuencia.
--
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas lst = map (\x -> (length $ filter (== x) lst, x)) (nub lst)

-- Ej 4 - Precond: texto no vacío
-- Dada una lista, calcula la cantidad promedio de repeticiones por cada palabra
--
repeticionesPromedio :: Extractor
repeticionesPromedio txt = let palabras = split ' ' txt
                               frecuenciaPalabras = map (\x -> fromIntegral (fst x)) (cuentas palabras)
                           in  mean frecuenciaPalabras

-- Ej 5
-- Devuelve un extractor por cada uno de los simbolos definidos en la constante tokens, 
-- con la frecuencia relativa de estos con respecto a todos los caracteres presentes en la lista.
-- A cada token se le mapea un Extractor que calcula su propia frecuencia respecto de la lista.
--
tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = map frec tokens
                   where frec t txt = let tokenFrec = filter ((== t) . snd) (cuentas txt)
                                      in if tokenFrec == [] 
                                         then 0
                                         else fromIntegral (fst (head tokenFrec)) / (genericLength txt)

-- Ej 6
-- Dado un extractor, lo modifica de manera que el valor de los features se encuentre entre -1 y 1 
-- para todos los datos con los que se dispone. Para ello, se obtiene el maximo valor absoluto y se
-- normaliza con dicho valor.
--
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor txt e = let eMax = maximum (map (abs . e) txt)
                            in  if eMax == 0 then (const 0) else (\t -> (e t) / eMax)

-- Ej 7
-- Permita aplicar varios extractores a todos los programas que se reciban como parametro
-- y de esta manera lograr obtener una matriz de atributos. Previamente se normalizan todos los extractores.
--
extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures es ts = let esNorm = map (normalizarExtractor ts) es
                        in [[ e t | e <- esNorm] | t <- ts]

-- Ej 8
-- Calcula la distancia Euclideana y la distancia Coseno entre dos vectores.
--
distEuclidiana :: Medida
distEuclidiana v1 v2 = sqrt $ sum $ map (**2) $ zipWith (-) v1 v2

distCoseno :: Medida
distCoseno v1 v2  = (prodVectorial v1 v2) / ((norma v1) * (norma v2))
                    where prodVectorial v1 v2 = sum $ zipWith (*) v1 v2
                          norma v1 = sqrt $ prodVectorial v1 v1
     
-- Ej 9  
-- Algoritmo de clasificacion. Dada una instancia calcula la distancia a todas las instancias de entrenamiento, 
-- se seleccionan las K instancias mas cercanos y se obtienen sus etiquetas. A esta lista de etiquetas, 
-- se calcula la moda estadistica y se etiqueta a la nueva instancia con esa moda.
-- Para calcular la moda, se calcula la frecuencia de las etiquetas y al ordenarlas se toma la primera de mayor frecuencia.
--
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn vecinos datos etiquetas medida instancia = moda $ take vecinos $ sort $ zipWith (\x y -> (medida instancia x, y)) datos etiquetas
                                               where moda lst = snd $ last $ sort $ cuentas $ map (\par -> snd par) lst

-- Ej 10
-- Separa los datos en datos de entrenamiento y datos de validacion.
-- Toma una matriz de datos xs y sus respectivas etiquetas y, luego dos numeros n y p, 
-- y devuelve el resultado de partir la matriz xs en n particiones dejando la particion numero p.
-- Para realizar la particion se toma la division entera de los datos y luego tomando multiplos de dicha
-- particion se opera con Take y Drop de manera adecuada para tomar los datos apropiados.
--
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas particiones reservada = let partLen = div (length datos) particiones
                                                         datosEntrenamiento = (take (partLen * (reservada - 1)) datos) ++ (take (partLen * (particiones - reservada)) $ (drop (partLen * reservada) datos))
                                                         etiquetasEntrenamiento = (take (partLen * (reservada - 1)) etiquetas) ++ (take (partLen * (particiones - reservada)) $ (drop (partLen * reservada) etiquetas))
                                                         datosValidacion = take partLen $ drop (partLen * (reservada - 1)) datos
                                                         etiquetasValidacion = take partLen $ drop (partLen * (reservada - 1)) etiquetas
                                                     in  (datosEntrenamiento, datosValidacion, etiquetasEntrenamiento, etiquetasValidacion)

-- Ej 11
-- Devuelve la proporcion de aciertos entre dos listas de etiqueta, siendo un acierto cuando coinciden las mismas
-- posiciones de ambas listas.
--
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy etiquetas predicciones = mean $ zipWith (\e p -> if e == p then 1 else 0) etiquetas predicciones 

-- Ej 12   
-- Se promedia la precisión obtenida a partir de cada particionado posible.
-- La funcion partitionAccuracy, para cada particionado, usa knn (con k=15)
-- para inferir un resultado y luego comparando con las etiquetas de validacion
-- correspondientes, se obtiene el accuaracy de la particion.
--                                                  
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation particiones datos etiquetas = mean [partitionAccuracy reservada | reservada <- [1..particiones]]
  where partitionAccuracy reservada = let (datosEntrenamiento, datosValidacion, etiquetasEntrenamiento, etiquetasValidacion) = separarDatos datos etiquetas particiones reservada
                                      in  accuracy etiquetasValidacion [(knn 15 datosEntrenamiento etiquetasEntrenamiento distEuclidiana) dv | dv <- datosValidacion]

-- Ej 13
-- Intentamos probar los clasicadores para otro dominio. 
-- En este caso, intentamos clasificar el genero de peliculas de cine por su titulo.
-- Realizamos la prueba con un grupo de aproximadamente 500 peliculas que se diveden en
-- diez generos distintos (Documentaries, Dramas, Comedies, Action & Adventure, Children,
-- Sci-Fi & Fantasy, Satires, Horror Movies, Monster Movies, Thrillers).
-- Con los mismo clasificadores del TP solo logramos un indice del 0.3148515
--
-- C:\runTitles
-- "Titulos de peliculas: 507 instancias"
-- "Generos: 10"
-- 0.3148515
-- "random value: 0.28205128205128205"
