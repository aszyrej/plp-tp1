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

distEuclideana :: Medida
distEuclideana v1 v2 = sqrt $ sum $ map (**2) $ zipWith (-) v1 v2

distCoseno :: Medida
distCoseno v1 v2  = (prodVectorial v1 v2) / ((norma v1) * (norma v2))

prodVectorial :: Instancia -> Instancia -> Float
prodVectorial v1 v2 = sum $ zipWith (*) v1 v2

norma :: Instancia -> Float
norma v1 = sqrt $ prodVectorial v1 v1

knn :: Int -> Datos-> [Etiqueta] -> Medida -> Modelo
knn = undefined

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = undefined

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos = undefined

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined
