{-# LANGUAGE OverloadedStrings#-}
---Usar string en vez del tipo de texto (Text o ByteString) que usa Aeson

module Cats.Atrib where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as B

import Cats.Types

-- Clase de tipos con Atributos y sus instancias

class Atributable a where
 info :: a -> Atrib
 modify:: a -> Atrib -> a

instance Atributable Atrib where
 info = id
 modify _ a = a

instance Atributable Cat where
 info = keyCat
 modify _ atr = Cat atr
 
instance Atributable Fc where
 info = keyFc
 modify (Fc _ s t) atr = Fc atr s t
 
instance Atributable Nt where
 info = keyNt
 modify (Nt _ s t) atr = Nt atr s t

instance Atributable Diag where
 info = keyD
 modify (Diag _ b) atr = Diag atr b


--Manejo de Atributos

reName::(Atributable a) =>  (String -> String) -> a -> a
reName fun o = modify o atr where
                               atr = Atrib (fun n) d
                               Atrib n d  = info o


seeDetails::(Atributable a) => a -> IO ()
seeDetails = B.putStrLn . encode . details . info

addDetail::(ToJSON a, Atributable b) => b -> String -> a -> b
addDetail o k v = modify o (add (info o) k v)
                    where add (Atrib n (Object d)) k v = let dp = Object $ HM.insert (T.pack k) (toJSON v) d
                                                      in Atrib n dp
                          add (Atrib n _) k v = let dp = Object $ HM.singleton (T.pack k) (toJSON v) 
                                                   in Atrib n dp

atrNotFound:: Maybe a -> Either String a 
atrNotFound (Just s) = Right s
atrNotFound Nothing = Left "Atributo no definido"

readDetail::(FromJSON a, Atributable b) => b -> String -> Either String a --para leer un detalle es necesario marcar el tipo esperado 
readDetail o k = read (info o) k
                  where read (Atrib n (Object d)) k = do v <- atrNotFound $ HM.lookup (T.pack k) d
                                                         parseEither parseJSON v
                        read (Atrib n _) k = Left "Los detalles de un atributo deben ser un objeto JSON"

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

-- Detalles componibles

data CompDet d = CompDet { tag::String, def::String -> d, lComp::[d] -> d, hComp::[(d,d,d)] -> d, vComp::[d] -> d}

readCD::(FromJSON d, Atributable b) => CompDet d -> b -> Either String d
readCD det o = readDetail o (tag det)

setCD::(ToJSON d, Atributable b) =>  CompDet d -> b -> d -> b
setCD det o v = addDetail o (tag det) v

getCD::(FromJSON d, Atributable b) => CompDet d -> b -> d
getCD det o = fromRight ((def det) (name $ info o)) (readCD det o)

mapCD::(ToJSON d, FromJSON d, Atributable b) => CompDet d -> (d -> d) -> b -> b
mapCD det fun o = setCD det o v where
                                 v = fun $ getCD det o

compCDL::(FromJSON d) => CompDet d -> Line -> d 
compCDL det (Line fcs) = (lComp det) $ map (getCD det) fcs 

compCDB::(FromJSON d) => CompDet d -> Band -> d 
compCDB det (Band nts) = (hComp det) $ [ ( (getCD det) nt, (compCDL det) $ Line $ sourceNt nt  , (compCDL det) $ Line $ targetNt nt ) | nt<-nts] 

compCDD::(FromJSON d) => CompDet d -> Diag -> d 
compCDD det (Diag _ bans) = (vComp det) $ map (compCDB det) bans


---- Detalle componible "label"
labelCD :: CompDet String
labelCD = CompDet "label" (\x-> x) concat (concat . (map (\(a,_,_)-> a ))) concat

getLabel:: (Atributable b)=> b -> String 
getLabel = getCD labelCD


---Detalle componible "draw"
drawCD :: CompDet Bool 
drawCD = CompDet "draw" (\_-> False) or (or . (map (\(a,_,_)-> a ))) or

draw:: (Atributable b)=> b -> Bool 
draw = getCD drawCD

drawLabel::(Atributable b)=> b -> String
drawLabel atr
         |draw atr  = getLabel atr
         |otherwise = ""
 


--Definición de Genericos

--- Atributos minimales
minAtr:: String -> Atrib
minAtr s =  Atrib s d  where
               d = object ["label".= s ,"draw".= True]
               
--- Categoría terminal
catTerm = Cat $Atrib { name = "", details = object ["draw".= False]} 

--- constructor generico de una Cat
cat:: String -> Cat
cat s = Cat $ mapCD labelCD (\x ->"\\mc{"++s++"}") (minAtr s) 

 -- Cat $ Atrib s d where 
 --        d = object ["label".= ("\\mc{"++s++"}") ,"draw".= True]

--- Constructor generico de un Fc
fc :: String -> Cat -> Cat  -> Fc
fc s a b = Fc (minAtr s) a b 

--- Constructor genérico de una Nt
nt:: String -> [Fc] -> [Fc] -> Nt
nt s a b = Nt (minAtr s) a b  
               
--- constructor genérico de un Obj
objAsFc:: String -> Cat -> Fc
objAsFc s cat = fc s catTerm cat 

morfAsNt :: (String, String, String) -> Cat -> Nt
morfAsNt (sf, sa, sb) cat = nt sf [objAsFc sa cat] [objAsFc sb cat]

--- Constructor genericos de un Diag
diag:: String -> [Band]-> Diag
diag s bans = Diag (minAtr s) bans


--- Constructor de la identidad
idAtr:: Atrib -> Atrib
idAtr (Atrib n d) = let new = addDetail (Atrib ("id@"++n) d) "draw" False
                        label = concat ["\\id{", getLabel new,"}"]
                    in addDetail new "label" label

idNt:: Fc -> Nt
idNt (Fc s cs ct) = Nt (idAtr s) [Fc s cs ct] [Fc s cs ct]

idFc:: Cat -> Fc
idFc (Cat s) = Fc (idAtr s) (Cat s) (Cat s)

ntDiag:: Nt -> Diag
ntDiag nt = diag  s [(Band [nt])] where
                s = "@"++ ((name . keyNt) nt)

