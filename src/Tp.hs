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
split e = foldr (\x rec -> if x == e then []:rec else (x:(head rec)):(tail rec)) [[]] 

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

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas cantParticiones valPart = (aplanar datosTrain, datosTest, aplanar etiqTrain, etiqTest)
  where 
        cantElemPorPart = div (length datos) cantParticiones
        datosRelevantes = take (cantElemPorPart * cantParticiones) datos
        etiqRelevantes = take (cantElemPorPart * cantParticiones) etiquetas
        
        particionesDatos = particionesDeLargoN cantElemPorPart datosRelevantes
        particionesEtiquetas = particionesDeLargoN cantElemPorPart etiqRelevantes
        
        valPartIndex = valPart-1
        (datosTest, datosTrain) = extraerElementoN valPartIndex particionesDatos
        (etiqTest, etiqTrain) = extraerElementoN valPartIndex particionesEtiquetas

{- Versión anterior:
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas cantParticiones particion = 
  (datos_train, datos_particion, etiquetas_train, etiquetas_particion)
  where 
        cantElemPorPart = div (length datos) cantParticiones
        p = particion-1
        n = cantParticiones*cantElemPorPart
        d = take n datos
        e = take n etiquetas
        
        datos_train = withoutSublist p cantElemPorPart d
        datos_particion = sublist p cantElemPorPart d
        etiquetas_train = withoutSublist p cantElemPorPart e
        etiquetas_particion = sublist p cantElemPorPart e


withoutSublist i len l = (take n l) ++ (drop m l)
  where n = i*len :: Int
        m = (i+1)*len :: Int

sublist i len list = take len $ drop n list   
  where n = i*len :: Int
-}

xs = [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]]
y = ["1","2","3","4","5","6","7"]
(datosTrain, datosTest, etiqTrain, etiqTest) = separarDatos xs y 3 2


-- Se podría re contra optimizar pasando las particiones correspondientes directamente, calculándolas en nFoldCrossValidation.
-- No usaríamos "separarDatos", pero... yo que sé. Correr el run en mi PC tarda infinito, lo dejo corriendo a la noche a ver
-- qué onda.
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = mean $ map (nFoldCrossValidationParticionN n datos etiquetas) [0..(length datos - 1)]

nFoldCrossValidationParticionN :: Int -> Datos -> [Etiqueta] -> Int -> Float
nFoldCrossValidationParticionN cantParticiones datos etiquetas valPart = 
    let (datosTrain, datosTest, etiqTrain, etiqTest) = separarDatos datos etiquetas cantParticiones valPart
    in accuracy (map (knn 15 datosTrain etiqTrain distEuclidiana) datosTest) etiqTest

