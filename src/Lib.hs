module Lib where

import Cats.Types
import Cats.Atrib

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
prima (Fc (Atrib s d) a b) = let atr = mapCD labelCD (\x -> x++"'") (Atrib (s ++"'") d)
                             in (Fc atr a b)

--- Trans. Naturales

ntPrima:: String -> Fc -> Nt
ntPrima s fc = nt s [fc] [(prima fc)]

ntAlpha = ntPrima "\\alpha" fcF
ntBeta = ntPrima "\\beta" fcG
ntGamma = ntPrima "\\gamma" fcH
ntUni = nt "\\eta" [(idFc catC)] [fcF, fcU]
ntEva = nt "\\varepsilon" [fcU, fcF] [(idFc catD)]
ntMu = nt "\\mu" [fcT, fcT] [fcT]
ntUn = nt "\\eta" [(idFc catC)] [fcT]
-- Algunos Diagramas

--- reglas de adujunción

adjF = diag "adjF" [b1,b2] where
          b1= Band [ntUni , idNt fcF]
          b2= Band [idNt fcF, ntEva] 

adjU = diag "adjU" [b1,b2] where
          b1= Band [idNt fcU, ntUni]
          b2= Band [ntEva , idNt fcU]

--- reglas de Monada

monUniL = diag "multiplicar por la unidad primero" [b1,b2] where
            b1 = Band [ntUn, idNt fcT]
            b2 = Band [ntMu]

monUniR = diag "multiplicar por la unidad despues" [b1,b2] where
            b1 = Band [idNt fcT, ntUn]
            b2 = Band [ntMu]

monAsoL = diag "Asosiatividad primero" [b1,b2] where
            b1 = Band [ntMu, idNt fcT]
            b2 = Band [ntMu]

monAsoR = diag "Asosiatividad despues" [b1,b2] where
            b1 = Band [idNt fcT, ntMu]
            b2 = Band [ntMu]


--- Objetos

objA = objAsFc "A" catC
objB = objAsFc "B" catC
objC = objAsFc "C" catC


objM = objAsFc "M" catD
objN = objAsFc "N" catD
objL = objAsFc "L" catD

objX = objAsFc "X" catE
objY = objAsFc "Y" catE
objZ = objAsFc "Z" catE



--- Morfismos

r = nt "r" [objX, fcT] [objX]

