module Cats.Gen 
    (
    catTerm
    , cat
    , fc
    , nt
    , obj
    , morf
    , diag
    , idFc
    , idNt
    , ntDiag
     -- Detalles implementados
    , labelCD
    , drawCD
    , getLabel
    , draw
    , drawLabel
     )
     where

import Cats.Types
import Cats.Atrib

-- Implementacion de Detalles

---- Detalle "label"
labelCD :: Det String
labelCD = Det "label" (\x-> x)

getLabel:: (Atributable b)=> b -> String 
getLabel = getCD labelCD


---Detalle "draw"
drawCD :: Det Bool 
drawCD = Det "draw" (\n -> not (take 3 n == "id@" || n =="@TeminalCat@"))

draw:: (Atributable b)=> b -> Bool 
draw = getCD drawCD

drawLabel::(Atributable b)=> b -> String
drawLabel atr
         |draw atr  = getLabel atr
         |otherwise = ""
 


--Definición de Genericos
               
--- Categoría terminal
catTerm = Cat $ addDetail (minAtr "@TeminalCat@") "draw" False
 
--- constructor generico de una Cat
cat:: String -> Cat Atrib
cat s = Cat $ mapCD labelCD (\x ->"\\mc{"++s++"}") (minAtr s) 

--- Constructor generico de un Fc
fc :: String -> Cat Atrib -> Cat Atrib  -> Fc Atrib
fc s a b = Fc (minAtr s) a b 

--- Constructor genérico de una Nt
nt:: String -> [(Fc Atrib)] -> [(Fc Atrib)] -> Nt Atrib
nt s a b =case  do  al <- toLine a
                    bl <- toLine b
                    return $ Nt (minAtr s) al bl
                of Just n -> n
                   Nothing -> Nt (minAtr s) (Line [] []) (Line [] [])
                   
--- constructor genérico de un Obj
obj:: String -> Cat Atrib -> Fc Atrib
obj s cat = fc s catTerm cat 

morf :: (String, String, String) -> Cat Atrib -> Nt Atrib
morf (sf, sa, sb) cat = nt sf [obj sa cat] [obj sb cat]

--- Constructor genericos de un Diag
diag:: String -> [Band Atrib] -> Diag Atrib
diag s bs = Diag (minAtr s) bs


--- Constructores de la identidad

idNt:: Fc Atrib -> Nt Atrib
idNt (Fc s cs ct) = Nt idAtr (Line [cs, ct] [s]) (Line [cs, ct] [s]) where 
                                   idAtr = let atr1 = mapCD labelCD (\l -> "\\id{" ++ l ++ "}" )  s
                                               atr2 = addDetail atr1 "draw" False
                                            in reName (\n -> "id@" ++ n ) atr2                                 

idFc:: Cat Atrib -> Fc Atrib
idFc (Cat s) = Fc idAtr (Cat s) (Cat s) where
                                   idAtr = let atr1 = mapCD labelCD (\l -> "\\Id{" ++ l ++ "}" )  s
                                               atr2 = addDetail atr1 "draw" False
                                            in reName (\n -> "id@" ++ n ) atr2


ntDiag:: Nt Atrib -> Diag  Atrib
ntDiag nt = diag  s [(Band [nt])] where
                s = "@"++ ((name . info) nt)

