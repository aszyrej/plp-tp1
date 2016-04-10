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
split :: Eq a => a -> [a] -> [[a]]
split e = filter (/= []) . foldr 
                    (\x rec -> if x == e then []:rec else (x:(head rec)):(tail rec))
                    [[]]

-- Auxiliar para separar texto en palabras.
palabras = split ' '

-- Ej2 - Precond: texto no vacío
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras txt = let longitudes = map genericLength (palabras txt)
                               in  mean longitudes

-- Ej 3
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas lst = map (\x -> (length $ filter (== x) lst, x)) (nub lst)

-- Ej 4 - Precond: texto no vacío
repeticionesPromedio :: Extractor
repeticionesPromedio txt = let frecuenciaPalabras = map (\x -> fromIntegral (fst x)) (cuentas (palabras txt))
                           in  mean frecuenciaPalabras

-- Ej 5
tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = map frec tokens
                   where frec t txt = let tokenFrec = filter ((== t) . snd) (cuentas txt)
                                      in if tokenFrec == [] 
                                         then 0
                                         else fromIntegral (fst (head tokenFrec)) / (genericLength txt)

-- Ej 6
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor txt e = let eMax = maximum (map (abs . e) txt)
                            in  if eMax == 0 then (const 0) else (\t -> (e t) / eMax)

-- Ej 7
extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures es ts = let esNorm = map (normalizarExtractor ts) es
                        in [[ e t | e <- esNorm] | t <- ts]

-- Ej 8
distEuclidiana :: Medida
distEuclidiana v1 v2 = sqrt $ sum $ map (**2) $ zipWith (-) v1 v2

distCoseno :: Medida
distCoseno v1 v2  = (prodVectorial v1 v2) / ((norma v1) * (norma v2))
                    where prodVectorial v1 v2 = sum $ zipWith (*) v1 v2
                          norma v1 = sqrt $ prodVectorial v1 v1
     
-- Ej 9  
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn vecinos datos etiquetas medida instancia = moda $ take vecinos $ sort $ zipWith (\x y -> (medida instancia x, y)) datos etiquetas
                                               where moda lst = snd $ head $ reverse $ sort $ cuentas $ map (\par -> snd par) lst

-- Precond: Lista de listas (xss) no vacía.
agregarAlPrimero :: a->[[a]]->[[a]]
agregarAlPrimero x xss = (x : (head xss)) : (tail xss)


-- Precond: n > 0 && (len xs) es mult de n.
particionesDeLargoN :: Int->[a]->[[a]]
particionesDeLargoN _ [] = []
particionesDeLargoN n xs = fst $    foldr (\x res -> 
                                    if (snd res) < n 
                                    then (agregarAlPrimero x (fst res),(snd res)+1)
                                    else ([x]:(fst res),1)) 
                                    ([[]], 0)
                                    xs

-- Auxiliar para obtener una tupla con el elemento N por un lado y el resto por el otro.
extraerElementoN :: Int -> [a] -> (a, [a])
extraerElementoN n xs = let ultimos = drop n xs
                        in (head ultimos, (take n xs) ++ (tail ultimos))

-- Auxiliar que, dados datos, etiquetas y cantPart deseadas, obtiene el particionado correspondientes, descartando sobrantes.
obtenerParticionesCorrespondientes :: Datos -> [Etiqueta] -> Int -> ([Datos], [[Etiqueta]])
obtenerParticionesCorrespondientes datos etiquetas cantParticiones = (particionesDatos, particionesEtiquetas)
    where
        cantElemPorPart = div (length datos) cantParticiones
        datosRelevantes = take (cantElemPorPart * cantParticiones) datos
        etiqRelevantes = take (cantElemPorPart * cantParticiones) etiquetas
        particionesDatos = particionesDeLargoN cantElemPorPart datosRelevantes
        particionesEtiquetas = particionesDeLargoN cantElemPorPart etiqRelevantes
          
-- Auxiliar que dadas las particiones y cuál quiero para validación, separa en (DatosTrain, DatosTest, EtiqTrain, EtiqTest).
separarParticiones :: [Datos] -> [[Etiqueta]] -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarParticiones particionesDatos particionesEtiquetas valPartIndex =
    (concat datosTrain, datosTest, concat etiqTrain, etiqTest)
    where
        (datosTest, datosTrain) = extraerElementoN valPartIndex particionesDatos
        (etiqTest, etiqTrain) = extraerElementoN valPartIndex particionesEtiquetas

-- Ej 10
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas cantParticiones valPart = 
    let valPartIndex = valPart-1
        (particionesDatos, particionesEtiquetas) = obtenerParticionesCorrespondientes datos etiquetas cantParticiones
    in separarParticiones particionesDatos particionesEtiquetas valPartIndex

-- Ej 11
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy etiquetas predicciones = mean $ zipWith (\e p -> if e == p then 1 else 0) etiquetas predicciones 

-- Ej 12                                                     
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation particiones datos etiquetas = mean [partitionAccuracy reservada | reservada <- [1..particiones]]
  where partitionAccuracy reservada = let (datosEntrenamiento, datosValidacion, etiquetasEntrenamiento, etiquetasValidacion) = separarDatos datos etiquetas particiones reservada
                                      in  accuracy etiquetasValidacion [(knn 15 datosEntrenamiento etiquetasEntrenamiento distEuclidiana) dv | dv <- datosValidacion]
