{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}

module Cats.Types 
   (
    -- Types
    Atrib (..)
    , Cat (..)
    , Fc (..)
    , Nt (..)
    , Line (..)
    , Band (..)
    , Diag (..)
    -- Classes
    , Composable (..)
    -- functions
    , equatable
    )
   where

import Data.Aeson
import GHC.Generics
import Data.List


---- Atrib contiene los atributos
data Atrib = Atrib {name::String,
                    details::Value --tipo de un  JSON
                    }deriving (Generic)

instance Eq Atrib where
   x == y = name x == name y ---ignora los otros atributos

isIdAtr:: Atrib -> Bool
isIdAtr (Atrib n _) = (take 3 n) == "id@" 



---definición de tipos

data Cat = Cat {keyCat::Atrib} deriving (Eq, Generic)
data Fc = Fc {keyFc::Atrib, sourceFc::Cat, targetFc::Cat} deriving (Eq, Generic)
data Nt = Nt {keyNt::Atrib, sourceNt::[Fc], targetNt::[Fc]} deriving (Eq, Generic)
 
newtype Line = Line {contentL::[Fc]} deriving (Generic) -- Linea horizontal de Fcs
newtype Band = Band {contentB::[Nt]} deriving (Eq, Generic) -- Banda horizontal de Nts
data Diag = Diag {keyD::Atrib, contentD::[Band]} deriving (Eq, Generic) -- lista vertical de Bandas


--- las lineas son iguales quitando identidades

removeIds:: [Fc] -> [Fc]
removeIds as = [a | a <- as,  (not . isIdAtr . keyFc) a  ]

instance Eq Line where
  (Line as) == (Line bs) = (removeIds as) == (removeIds bs)


--Clase de componibilidad globular
class (Eq b) => Composable a b | a -> b where
   valid:: a -> Bool
   source:: a -> b
   target:: a -> b
 --  Métodos para verificar una correcta componibilidad
   check:: a -> a -> Bool
   check a1 a2 = and [valid a1, valid a2, (target a1 == source a2)]
   checkList:: [a] -> Bool
   checkList [] = False
   checkList ls = and $ zipWith check (init ls) (tail ls)


instance Composable Fc Cat where
    valid _ = True
    source = sourceFc
    target = targetFc

instance Composable Line Cat where
    valid (Line lsFc) = checkList lsFc
    source (Line lsFc) = source $ head lsFc
    target (Line lsFc) = target $ last lsFc

instance Composable Nt Cat where -- horizontal
   valid (Nt  _ alsFc blsFc) = and [ valid aLn , valid bLn, source aLn == source bLn, target aLn == target bLn] where
                                    aLn = Line alsFc
                                    bLn = Line blsFc
   source (Nt  _ alsFc blsFc) =  source $ Line alsFc
   target (Nt  _ alsFc blsFc) =  target $ Line alsFc

instance Composable Band Line where -- Vertical
   valid (Band lsNt)  = checkList lsNt
   source (Band lsNt) = Line $ concat (map sourceNt lsNt)
   target (Band lsNt) = Line $ concat (map targetNt lsNt)

instance Composable Diag Line where -- Vertical
   valid (Diag _ lsB) = checkList lsB
   source (Diag _ lsB) = source $ head lsB
   target (Diag _ lsB) = target $ last lsB

allTheSame :: (Eq b) => [b] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

equatable::(Composable a b) => [a] -> Bool
equatable [] = False
equatable as = and $ [allTheSame (map source as), allTheSame (map target as)] ++ (map valid as)


-- show instances

instance Show Atrib where
   show = name

instance Show Cat where
   show (Cat k) = "("++ (show k) ++ ")"

instance Show Fc where
   show (Fc k s t) = (show s) ++ "--" ++ (show k) ++ "-->" ++ (show t)

instance Show Nt where
   show (Nt k s t) = (show $ Line s)++"==" ++ (show k)++ "==>" ++ (show $ Line t)

instance Show Line where
   show (Line fcs) = intercalate ":" $ map show fcs

instance Show Band where
   show (Band nts) = intercalate ";" $ map show nts

instance Show Diag where
   show (Diag k bans) = "Diag "++ (show k)++"\n"++(intercalate "\n" $ map show bans)


--- escribir y leer Json

instance FromJSON Atrib
instance ToJSON Atrib

instance FromJSON Cat
instance ToJSON Cat

instance FromJSON Fc
instance ToJSON Fc

instance FromJSON Nt
instance ToJSON Nt

instance FromJSON Line
instance ToJSON Line

instance FromJSON Band
instance ToJSON Band

instance FromJSON Diag
instance ToJSON Diag
