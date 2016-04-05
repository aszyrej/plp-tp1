-- Para correr los tests:
-- ghc Tests-alu.hs && ./Tests-alu

import Tp
import Test.HUnit
import Data.List

-- evaluar main para correr todos los tests
main = runTestTT allTests

allTests = test [
    "e1split" ~: testsE1Split,
    "e2longPromedio" ~: testsE2LongPromedio,
    "e3cuentas" ~: testsE3Cuentas,
    "e4repPromedio" ~: testsE4RepPromedio,
    "e5frecTokens" ~: testsE5FrecTokens,
    "e6normExtractor" ~: testsE6NormExtractor,
    "e7extraerFeatures" ~: testsE7ExtraerFeatures,
    "e8aDistanciaEuclidiana" ~: testsE8aDistanciaEuclidiana,
    "e8bDistanciaCoseno" ~: testsE8bDistanciaCoseno,
    "e9knn" ~: testsE9KNN,
    "e10separarDatos" ~: testsE10SepararDatos,
    "e11accuracy" ~: testsE11Accuracy
    ]

testsE1Split = test [
--   split ',' ",PLP," ~?= ["PLP"], De la cátedra. No parece correcto.
    split ',' ",PLP," ~?= ["", "PLP", ""],
    split ',' " ,PLP, " ~?= [" ","PLP"," "],
    split ',' "" ~?= [""],
    split ',' ",," ~?= ["","",""]
    ]

testsE2LongPromedio = test [
    longitudPromedioPalabras "" ~?= 0.0,
    longitudPromedioPalabras " " ~?= 0.0,
    longitudPromedioPalabras "cinco" ~?= 5.0,
    longitudPromedioPalabras "palabras v4r14s ??$ !" ~?= 4.5
    ]
    
testsE3Cuentas = test [
    cuentas [""] ~?= [(1, "")],
    cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")]
    ]

testsE4RepPromedio = test [
    repeticionesPromedio "" ~?= 0.0,
    repeticionesPromedio "1 2 3" ~?= 1.0,
    repeticionesPromedio "uno dos dos" ~?= 1.5,
    repeticionesPromedio "dos tres dos tres tres" ~?= 2.5
    ]
    
testsE5FrecTokens = test [ -- (head frecuenciaTokens) es el extractor de "_"
    (head frecuenciaTokens) "" ~?= 0.0,
    (head frecuenciaTokens) "no aparece" ~?= 0.0,
    (head frecuenciaTokens) "_" ~?= 1.0,
    (head frecuenciaTokens) "______" ~?= 1.0,
    (head frecuenciaTokens) "o__O" ~?= 0.5,
    (head frecuenciaTokens) "  _  " ~?= 0.2
    ]
    
testsE6NormExtractor = test [
    normalizarExtractor ["", "2 2"] repeticionesPromedio "3 3 3" ~?= 1.5,
    normalizarExtractor ["1", "3 3 3", "5 5 5 5 5"] repeticionesPromedio "2 2 3 3 3" ~?= 0.5
    ]
    
testsE7ExtraerFeatures = test [
    extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ["b=a", "a = 2; a = 4", "C:/DOS C:/DOS/RUN RUN/DOS/RUN"] ~?= [[0.33333334,0.6666667],[0.12962963,1.0],[1.0,0.6666667]] -- Ejemplo del enunciado
    ]
    
testsE8aDistanciaEuclidiana = test [
    distEuclidiana [0,0] [0,0] ~?= 0.0,
    distEuclidiana [1,0] [0,0] ~?= 1.0,
    distEuclidiana [0,1] [0,0] ~?= 1.0,
    distEuclidiana [3,2] [0,6] ~?= 5.0,
    distEuclidiana [1.0,0.75,0.8125] [0.75,1.0,0.5] ~?= 0.47186464 -- Ejemplo del enunciado
    ]

testsE8bDistanciaCoseno = test [
    distCoseno [1,1] [5,5] ~?= 1.0,
    distCoseno [0,3,4] [0,-3,-4] ~?= -1.0 -- Ejemplo del enunciado
    ]

testsE9KNN = test [
        (knn 3 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distEuclidiana) [1,1] ~?= "f", -- Ejemplo del enunciado, con k=3. Con k=2 queda una "f" ([1,1]) y una "i" ([0,1] está tan cerca como [2,1]), ambas son moda.
        (knn 1 [[0], [1], [2], [5], [6], [7], [8]] ["a", "a", "a", "b", "a", "a", "a"] distEuclidiana) [4] ~?= "b",
        (knn 4 [[0], [1], [2], [3], [4], [5], [6]] ["a", "a", "a", "b", "b", "c", "d"] distEuclidiana) [10] ~?= "b",
        -- DistCoseno es menor cuanto más lejos estás (-1).
        (knn 1 [[1,1], [1,-1], [-1,-1], [-1, 1]] ["cuad3", "cuad4", "cuad1", "cuad2"] distCoseno) [3,7] ~?= "cuad1",
        (knn 1 [[1,1], [1,-1], [-1,-1], [-1, 1]] ["cuad3", "cuad4", "cuad1", "cuad2"] distCoseno) [10,-3] ~?= "cuad2",
        (knn 1 [[1,1], [1,-1], [-1,-1], [-1, 1]] ["cuad3", "cuad4", "cuad1", "cuad2"] distCoseno) [-3,-7] ~?= "cuad3",
        (knn 1 [[1,1], [1,-1], [-1,-1], [-1, 1]] ["cuad3", "cuad4", "cuad1", "cuad2"] distCoseno) [-35,11] ~?= "cuad4"
    ]

testsE10SepararDatos = test [
        separarDatos [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] ["1","2","3","4","5","6","7"] 3 2 ~?= ([[1.0,1.0],[2.0,2.0],[5.0,5.0],[6.0,6.0]],[[3.0,3.0],[4.0,4.0]],["1","2","5","6"],["3","4"]), -- Ejemplo del enunciado
        separarDatos [[1], [2], [3]] ["1", "2", "3"] 3 1 ~?= ([[2],[3]], [[1]], ["2", "3"], ["1"])
    ]

testsE11Accuracy = test [
        accuracy [""] [""] ~?= 1.0,
        accuracy ["1", "2"] ["2", "2"] ~?= 0.5,
        accuracy ["a", "b", "c", "d", "e"] ["e", "d", "c", "b", "a"] ~?= 0.2
    ]

