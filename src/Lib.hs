--- Librería con algunos elementos de uso común

module Lib where

import Cats.Types
import Cats.Atrib
import Cats.Gen


--- Categorias

catC = cat "C"
catD = cat "D"
catE = addDetail (cat "E") "color" "red" 
catConj = addDetail (cat "Conj") "label" "\\Conj"

--- Funtores

fcF = fc "F" catC catD
fcU = fc "U" catD catC
fcT = fc "T" catC catC
fcG = fc "G" catD catE
fcH = fc "H" catE catC

prima:: Fc -> Fc
prima f = let f1 = mapCD labelCD (\x -> x++"'") f
          in  reName (\x -> x++"'") f1

--- Trans. Naturales

ntPrima:: String -> Fc -> Nt
ntPrima s f = nt s [f] [(prima f)]

ntAlpha = ntPrima "\\alpha" fcF
ntBeta = ntPrima "\\beta" fcG
ntGamma = ntPrima "\\gamma" fcH
ntUni = nt "\\eta" [(idFc catC)] [fcF, fcU]
ntEva = nt "\\varepsilon" [fcU, fcF] [(idFc catD)]
ntMu = nt "\\mu" [fcT, fcT] [fcT]
ntUn = nt "\\eta" [(idFc catC)] [fcT]

-- Diagramas

--- reglas de adujunción

adjF = diag "Adjunto izquierdo" [b1,b2] where
          b1= Band [ntUni , idNt fcF]
          b2= Band [idNt fcF, ntEva] 

adjU = diag "Adjunto derecho" [b1,b2] where
          b1= Band [idNt fcU, ntUni]
          b2= Band [ntEva , idNt fcU]

--- reglas de Monada

monUniL = diag "Multiplicar por la unidad primero" [b1,b2] where
            b1 = Band [ntUn, idNt fcT]
            b2 = Band [ntMu]

monUniR = diag "Multiplicar por la unidad despues" [b1,b2] where
            b1 = Band [idNt fcT, ntUn]
            b2 = Band [ntMu]

monAsoL = diag "Asosiatividad primero" [b1,b2] where
            b1 = Band [ntMu, idNt fcT]
            b2 = Band [ntMu]

monAsoR = diag "Asosiatividad despues" [b1,b2] where
            b1 = Band [idNt fcT, ntMu]
            b2 = Band [ntMu]


--- Objetos

objA = obj "A" catC
objB = obj "B" catC
objC = obj "C" catC


objM = obj "M" catD
objN = obj "N" catD
objL = obj "L" catD

objX = obj "X" catE
objY = obj "Y" catE
objZ = obj "Z" catE


--- Morfismos

r = nt "r" [objX, fcT] [objX]

