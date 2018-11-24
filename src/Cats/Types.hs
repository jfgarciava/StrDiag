{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}

module Cats.Types 
   (
    -- Types
    Atrib (..)
    , Cat (..)
    , Fc (..)
    , Nt (..)
    , NNt (..)
    , Line (..)
    , NLine (..)
    , Band (..)
    , Diag (..)
    -- Classes
    , Composable (..)
    -- functions
    , equatable
    , toLine
    , contentNL
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

---definición de tipos

data Cat = Cat {keyCat::Atrib} deriving (Eq, Generic)
data Fc = Fc {keyFc::Atrib, sourceFc::Cat, targetFc::Cat} deriving (Eq, Generic)

data NLine = NLine {catsL :: [Cat], fcsAtr::[Atrib] } deriving (Eq, Generic) --Ocupa la mitad de almacenamiento
newtype Line = Line {contentL::[Fc]} deriving (Eq, Generic) -- Linea horizontal de Fcs

data Nt = Nt {keyNt::Atrib, sourceNt::[Fc], targetNt::[Fc]} deriving (Eq, Generic)
data NNt = NNt {keyNNt::Atrib, sourceNNt::NLine, targetNNt::NLine} deriving (Eq, Generic)

newtype Band = Band {contentB::[Nt]} deriving (Eq, Generic) -- Banda horizontal de Nts
data Diag = Diag {keyD::Atrib, contentD::[Band]} deriving (Eq, Generic) -- lista vertical de Bandas



toLine:: [Fc] -> Maybe NLine
toLine [] = Nothing
toLine fs = let as = map keyFc fs
                ss = (map sourceFc fs) ++ [targetFc $ last fs]
                ts = [sourceFc $ head fs] ++ (map targetFc fs)
             in if ss == ts then Just $ (NLine ss as) else Nothing
                                                              
contentNL:: NLine -> [Fc]
contentNL (NLine [] _) = [] -- Caso incorrecto
contentNL (NLine _ []) = []
contentNL (NLine (s:t:cs) (f:fs)) = (Fc f s t) : (contentNL (NLine (t:cs) fs)) 


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

instance Composable NLine Cat where
    valid l = let n = length (fcsAtr l) in (length (catsL l) == n+1) && (n > 0)
    source l = head $ catsL l
    target l = last $ catsL l


instance Composable Nt Cat where -- horizontal
   valid (Nt  _ alsFc blsFc) = and [ valid aLn , valid bLn, source aLn == source bLn, target aLn == target bLn] where
                                    aLn = Line alsFc
                                    bLn = Line blsFc
   source (Nt  _ alsFc blsFc) =  source $ Line alsFc
   target (Nt  _ alsFc blsFc) =  target $ Line alsFc

instance Composable NNt Cat where -- horizontal
   valid (NNt  _ s t) = and [ valid s , valid t, source s == source t, target s == target t] 
   source (NNt  _ s t) =  source s
   target (NNt  _ s t) =  target t

instance Composable Band Line where -- Vertical
   valid (Band lsNt)  = checkList lsNt
   source (Band lsNt) = Line $ concat (map sourceNt lsNt)
   target (Band lsNt) = Line $ concat (map targetNt lsNt)

instance Composable Diag Line where -- Vertical
   valid (Diag _ lsB) = checkList lsB
   source (Diag _ lsB) = source $ head lsB
   target (Diag _ lsB) = target $ last lsB

allTheSame:: (Eq b) => [b] -> Bool
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

instance Show Line where
   show (Line fcs) = intercalate ":" $ map show fcs

instance Show NLine where
   show (NLine [] e) = "@Error: Linea sin final. Sobran " ++ show e
   show (NLine [t] []) = show t
   show (NLine (t:e) []) = show t ++ " @Error:Cats sobrantes " ++ show e
   show (NLine (s:cs) (a:as)) = (show s) ++ "--" ++ (show a) ++ "-->" ++ show (NLine cs as)

instance Show Nt where
   show (Nt k s t) = (show $ Line s)++"==" ++ (show k)++ "==>" ++ (show $ Line t)

instance Show NNt where
   show (NNt k s t) = (show s)++"\n || \n" ++ (show k)++ "\n || \n \\ / \n" ++ (show t)

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
