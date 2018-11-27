module Cats.Gen 
    (
    catTerm
    , cat
    , fc
    , nt
    , obj
    , morf
    , diag
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
nt s a b=  Nt (minAtr s) al bl where  al = toLine a
                                      bl = toLine b
                   
--- constructor genérico de un Obj
obj:: String -> Cat Atrib -> Fc Atrib
obj s cat = fc s catTerm cat 

morf :: (String, String, String) -> Cat Atrib -> Nt Atrib
morf (sf, sa, sb) cat = nt sf [obj sa cat] [obj sb cat]

--- Constructor genérico de un Diagram
diag:: String -> [Band Atrib] -> Diagram Atrib
diag s bs = P (minAtr s) (Plane bs)


--- Constructores de la identidad

instance IdEq Atrib where
  idFcGen atr =  let atr1 = mapCD labelCD (\l -> "\\Id{" ++ l ++ "}" )  atr
                     atr2 = addDetail atr1 "draw" False
                 in reName (\n -> "id@" ++ n ) atr2

  idNtGen (Fc atr _ _) = let atr1 = mapCD labelCD (\l -> "\\id{" ++ l ++ "}" )  atr
                             atr2 = addDetail atr1 "draw" False
                         in reName (\n -> "id@" ++ n ) atr2

  isId atr = take 3 (name atr) == "id@"
