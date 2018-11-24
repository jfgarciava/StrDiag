module Cats.Atrib
     (
      --Types
      Det (..)
      --Classes
      , Atributable (..)
      -- functions
      -- on Atributes
      , minAtr
      , reName
      , seeDetails
      , addDetail
      , readDetail
      -- on details
      , readCD
      , setCD
      , getCD
      , mapCD
      --
      )
      where

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

--- Atributos minimales
minAtr:: String -> Atrib
minAtr s =  Atrib s d  where
                      d = object []


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

-- Detalles 

data Det d = Det { tag::String, def::String -> d}--, lComp::[d] -> d, hComp::[(d,d,d)] -> d, vComp::[d] -> d}

readCD::(FromJSON d, Atributable b) => Det d -> b -> Either String d
readCD det o = readDetail o (tag det)

setCD::(ToJSON d, Atributable b) =>  Det d -> b -> d -> b
setCD det o v = addDetail o (tag det) v

getCD::(FromJSON d, Atributable b) => Det d -> b -> d
getCD det o = fromRight ((def det) (name $ info o)) (readCD det o)

mapCD::(ToJSON d, FromJSON d, Atributable b) => Det d -> (d -> d) -> b -> b
mapCD det fun o = setCD det o v where
                                 v = fun $ getCD det o

