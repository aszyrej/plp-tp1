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

longitudPromedioPalabras :: Extractor
longitudPromedioPalabras txt = let palabras = filter (/= []) (split ' ' txt)
                                   longitudes = map length palabras
                                   cantidad = length palabras
                               in  fromIntegral (sum longitudes) / fromIntegral cantidad

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas lst = map (\x -> (length (filter (== x) lst), x)) (nub lst)

repeticionesPromedio :: Extractor
repeticionesPromedio txt = let frecuenciaPalabras = map (\x -> fst x) (cuentas (split ' ' txt))
                               cantidad = length frecuenciaPalabras
                           in  fromIntegral (sum frecuenciaPalabras) / fromIntegral cantidad

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
extraerFeatures = undefined

distEuclideana :: Medida
distEuclideana = undefined

distCoseno :: Medida
distCoseno = undefined

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = undefined

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = undefined

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos = undefined

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined
