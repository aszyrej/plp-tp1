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

split :: Eq a => a -> [a] -> [[a]]
split e str = trimSplit $ foldr (\x rec -> if x == e then []:rec else (x:(head rec)):(tail rec)) [[]] str

trimSplit :: Eq a => [[a]] -> [[a]]
trimSplit = reverse . dropWhile null . reverse . dropWhile null

-- Auxiliar.
palabras txt = filter (/= []) (split ' ' txt)

-- Separamos el caso 0.0, si no, da NaN.
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras txt = longitudPromedioPalabrasSeparadas (palabras txt)

longitudPromedioPalabrasSeparadas :: [Texto]->Float
longitudPromedioPalabrasSeparadas [] = 0.0
longitudPromedioPalabrasSeparadas plbrs =   let longitudes = map genericLength plbrs
                                            in  mean longitudes

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas lst = map (\x -> (length $ filter (== x) lst, x)) (nub lst)

{- Intento de optimización sacando en cada pasada los x que se contaron para no volver a recorrerlos.
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas lst = fst $ foldr (\x res -> ((contarAparicionesDe x (snd res), x):(fst res), filter (/= x) (snd res))) ([], lst) (nub lst)

contarAparicionesDe :: Eq a => a -> [a] -> Int
contarAparicionesDe x = length . filter (== x) 
-}

-- Mismo que longPromedio, separamos el caso 0.0, si no, da NaN.
repeticionesPromedio :: Extractor
repeticionesPromedio txt = repeticionesPromedioPalabrasSeparadas (palabras txt)

repeticionesPromedioPalabrasSeparadas [] = 0.0
repeticionesPromedioPalabrasSeparadas plbrs =   let frecuenciaPalabras = map (\x -> fromIntegral $ fst x) (cuentas plbrs)
                                                in mean frecuenciaPalabras
tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

frecuenciaTokens :: [Extractor]
frecuenciaTokens = map frec tokens
                   where frec t txt = let tokenFrec = filter ((== t) . snd) (cuentas txt)
                                      in if tokenFrec == [] 
                                         then 0
                                         else fromIntegral (fst (head tokenFrec)) / fromIntegral (length txt)

										 {- Intento de optimización contando las apariciones de t en vez de calculando las de todos los símbolos.
frecuenciaTokens :: [Extractor]
frecuenciaTokens =  let frec = (\t txt -> frecuenciaToken t txt)
                    in  map frec tokens

frecuenciaToken :: Char -> Texto -> Float
frecuenciaToken t txt = let tokenFrec = fromIntegral $ contarAparicionesDe t txt
                            totalLength = fromIntegral $ length txt
                        in  if totalLength == 0 then 0.0 else tokenFrec / totalLength
-}

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor txt e = (\t -> (e t) / maximum (map (abs . e) txt))

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures es ts = let esNorm = map (normalizarExtractor ts) es
                        in [[ e t | e <- esNorm] | t <- ts]

distEuclidiana :: Medida
distEuclidiana v1 v2 = sqrt $ sum $ map (**2) $ zipWith (-) v1 v2

distCoseno :: Medida
distCoseno v1 v2  = (prodVectorial v1 v2) / ((norma v1) * (norma v2))

prodVectorial :: Instancia -> Instancia -> Float
prodVectorial v1 v2 = sum $ zipWith (*) v1 v2

norma :: Instancia -> Float
norma v1 = sqrt $ prodVectorial v1 v1

knn :: Int -> Datos-> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas distancia = 
    \instancia -> 
      moda $  
        map fst $ 
          take k $ 
            sortBy cmp $ 
              zip etiquetas $ 
                map (distancia instancia) datos

moda l = fst $ maximumBy (cmp) elemCounts where
    elemCounts = nub [(element, count) | element <- l, let count = length (filter (==element) l)]

cmp a b = compare (snd a) (snd b)

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy etiquetas predicciones = mean $ zipWith (\a b -> if a == b then 1.0 else 0.0) etiquetas predicciones

-- Precond: n > 0 && (len xs) es mult de n.
particionesDeLargoN :: Int->[a]->[[a]]
particionesDeLargoN _ [] = []
particionesDeLargoN n xs = fst $    foldr (\x res -> 
                                    if (snd res) < n 
                                    then (agregarAlPrimero x (fst res),(snd res)+1)
                                    else ([x]:(fst res),1)) 
                                    ([[]], 0)
                                    xs

-- Precond: Lista de listas (xss) no vacía.
agregarAlPrimero :: a->[[a]]->[[a]]
agregarAlPrimero x xss = (x : (head xss)) : (tail xss)

-- Auxiliar: Lista de lisas -> se concatenan todas en una única lista
aplanar = foldr (++) []

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
    (aplanar datosTrain, datosTest, aplanar etiqTrain, etiqTest)
    where
        (datosTest, datosTrain) = extraerElementoN valPartIndex particionesDatos
        (etiqTest, etiqTrain) = extraerElementoN valPartIndex particionesEtiquetas

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas cantParticiones valPart = 
    let valPartIndex = valPart-1
        (particionesDatos, particionesEtiquetas) = obtenerParticionesCorrespondientes datos etiquetas cantParticiones
    in separarParticiones particionesDatos particionesEtiquetas valPartIndex

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas =
    let (particionesDatos, particionesEtiquetas) = obtenerParticionesCorrespondientes datos etiquetas n
    in mean $ map (nFoldCrossValidationParticionK particionesDatos particionesEtiquetas) [0..(length particionesDatos - 1)]

nFoldCrossValidationParticionK :: [Datos] -> [[Etiqueta]] -> Int -> Float
nFoldCrossValidationParticionK particionesDatos particionesEtiquetas k = 
    let (datosTrain, datosTest, etiqTrain, etiqTest) = separarParticiones particionesDatos particionesEtiquetas k
    in accuracy (map (knn 15 datosTrain etiqTrain distEuclidiana) datosTest) etiqTest

