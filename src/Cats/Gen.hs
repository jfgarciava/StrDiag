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
     )
     where

import Cats.Types
import Cats.Atrib


--Definición de Genericos
               
--- Categoría terminal
catTerm = Cat $ addDetail (minAtr "") "draw" False

--- constructor generico de una Cat
cat:: String -> Cat
cat s = Cat $ mapCD labelCD (\x ->"\\mc{"++s++"}") (minAtr s) 

--- Constructor generico de un Fc
fc :: String -> Cat -> Cat  -> Fc
fc s a b = Fc (minAtr s) a b 

--- Constructor genérico de una Nt
nt:: String -> [Fc] -> [Fc] -> Nt
nt s a b = Nt (minAtr s) a b  
               
--- constructor genérico de un Obj
obj:: String -> Cat -> Fc
obj s cat = fc s catTerm cat 

morf :: (String, String, String) -> Cat -> Nt
morf (sf, sa, sb) cat = nt sf [obj sa cat] [obj sb cat]

--- Constructor genericos de un Diag
diag:: String -> [Band] -> Diag
diag s bans = Diag (minAtr s) bans


--- Constructores de la identidad

idNt:: Fc -> Nt
idNt (Fc s cs ct) = Nt idAtr [Fc s cs ct] [Fc s cs ct] where 
                                   idAtr = let atr1 = mapCD labelCD (\l -> "\\id{" ++ l ++ "}" )  s
                                               atr2 = addDetail atr1 "draw" False
                                            in reName (\n -> "id@" ++ n ) atr2                                 

idFc:: Cat -> Fc
idFc (Cat s) = Fc idAtr (Cat s) (Cat s) where
                                   idAtr = let atr1 = mapCD labelCD (\l -> "\\Id{" ++ l ++ "}" )  s
                                               atr2 = addDetail atr1 "draw" False
                                            in reName (\n -> "id@" ++ n ) atr2


ntDiag:: Nt -> Diag
ntDiag nt = diag  s [(Band [nt])] where
                s = "@"++ ((name . info) nt)

