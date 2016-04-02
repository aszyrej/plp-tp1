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
    "e7extraerFeatures" ~: testsE7ExtraerFeatures
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
    
testsE7ExtraerFeatures = test [ -- El ejemplo del enunciado
    extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ["b=a", "a = 2; a = 4", "C:/DOS C:/DOS/RUN RUN/DOS/RUN"] ~?= [[0.33333334,0.6666667],[0.12962963,1.0],[1.0,0.6666667]]
    ]