-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}

module Cats.Types 
   (
    -- Types
      Cat (..)
    , Fc (..)
    , Nt (..)
    , Line (..)
    , Band (..)
    , Plane (..)
    , Diagram (..)
    -- Classes
    , Composable (..)
    -- functions
    , equatable
    , toLine
    , contentL
    , central
    )
   where

-- import Data.Aeson
-- import GHC.Generics
import Data.List

import Cats.Atrib

---definición de tipos como funtores

data Cat a = Cat {keyCat::a} deriving (Eq) --, Generic)

instance Functor Cat where
  fmap f (Cat a) = Cat (f a)

data Fc a = Fc {keyFc::a, sourceFc::Cat a, targetFc::Cat a} deriving (Eq) -- , Generic)

instance Functor Fc where
  fmap f (Fc a s t) = Fc (f a) (fmap f s) (fmap f t)

data Line a = Line {catsL :: [Cat a], fcsAtr::[a] } deriving (Eq) -- , Generic)  -- Linea horizontal de Fcs

instance Functor Line where
  fmap f (Line cs as) = Line [fmap f c | c <- cs]  (map f as)

instance Semigroup (Line a) where
  (Line cs as) <> (Line ds bs) = Line (cs ++ drop 1 ds) (as ++ bs)

instance Monoid (Line a) where
  mempty = Line [] []

toLine:: (Eq a) => [Fc a] -> Line a
toLine [] = Line [] []
toLine fs = let as = map keyFc fs
                ss = (map sourceFc fs) ++ [targetFc $ last fs]
                ts = [sourceFc $ head fs] ++ (map targetFc fs)
             in if ss == ts then (Line ss as) else Line [] []
                                                              
contentL:: Line a -> [Fc a]
contentL (Line [] _) = [] -- Caso incorrecto
contentL (Line _ []) = []
contentL (Line (s:t:cs) (f:fs)) = (Fc f s t) : (contentL (Line (t:cs) fs)) 


data Nt a = Nt {keyNt::a, sourceNt::Line a, targetNt::Line a} deriving (Eq) --, Generic)

instance Functor Nt where
  fmap f (Nt a s t) = Nt (f a) (fmap f s) (fmap f t)
  
newtype Band a = Band {contentB::[Nt a]} deriving (Eq) ---, Generic) -- Banda horizontal de Nts

instance Functor Band where
  fmap f (Band ns) = Band [fmap f n | n <-ns]

instance Semigroup (Band a) where
  (Band  as) <> (Band bs) = Band (as ++ bs)

instance Monoid (Band a) where
  mempty = Band [] 
 
data Plane a = Plane { contentP::[Band a] } deriving (Eq) --, Generic) -- lista vertical de Bandas

instance Functor Plane where
 fmap f (Plane  bs) = Plane  [fmap f b | b <-bs]

instance  Semigroup (Plane a) where
  (Plane  as) <> (Plane  bs) = Plane (as ++ bs)

instance Monoid (Plane a) where
  mempty = Plane [] 

--- Incorrecto porque falta implementar id
hcomp ::Plane a -> Plane a -> Plane a
(Plane  as) `hcomp` (Plane  bs) = Plane (zipWith (<>) (as ++ (take m $ repeat mempty )) (bs ++ (take n $ repeat mempty )) ) where
                                                                                                                             m0= length as
                                                                                                                             n0= length bs
                                                                                                                             mm= max n0 m0
                                                                                                                             m = mm - m0
                                                                                                                             n = mm - n0

 
data Diagram a = C (Cat a) | F (Fc a) | N (Nt a) | L Atrib (Line a) | B Atrib (Band a) | P Atrib (Plane a) 

whichKind ::Diagram a -> String
whichKind (C _) = "Cat"
whichKind (F _) = "Fc"
whichKind (N _) = "Nt"
whichKind (L _ _) = "Line"
whichKind (B _ _) = "Band"
whichKind (P _ _) = "Plane"





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


instance (Eq a) => Composable (Fc a)  (Cat a) where
    valid _ = True
    source = sourceFc
    target = targetFc


instance (Eq a) => Composable (Line a) (Cat a)  where
    valid l = let n = length (fcsAtr l) in (length (catsL l) == n+1) && (n > 0)
    source l = head $ catsL l
    target l = last $ catsL l


instance (Eq a) => Composable (Nt a) (Cat a) where -- horizontal
   valid (Nt  _ s t) = and [ valid s , valid t, source s == source t, target s == target t] 
   source (Nt  _ s t) =  source s
   target (Nt  _ s t) =  target t

instance (Eq a) => Composable (Band a) (Line a) where -- Vertical
   valid (Band lsNt)  = checkList lsNt
   source (Band lsNt) = mconcat (map sourceNt lsNt) 
   target (Band lsNt) = mconcat (map targetNt lsNt)

central::(Eq a) => Band a -> [Cat a] 
central (Band []) = []
central (Band (n:ns)) = (source n):(map target (n:ns))


instance (Eq a) => Composable (Plane a) (Line a) where -- Vertical
   valid (Plane lsB) = checkList lsB
   source (Plane lsB) = source $ head lsB
   target (Plane lsB) = target $ last lsB

allTheSame:: (Eq b) => [b] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

equatable::(Composable a b) => [a] -> Bool
equatable [] = False
equatable as = and $ [allTheSame (map source as), allTheSame (map target as)] ++ (map valid as)

-- Atributable instances

instance (Atributable a) => Atributable (Cat a) where
 info = info . keyCat
 modify s atr = fmap (\x -> modify x atr) s
 
instance (Atributable a) => Atributable (Fc a) where
 info = info . keyFc
 modify s atr = fmap (\x -> modify x atr) s
 
instance (Atributable a) => Atributable (Nt a) where
 info = info . keyNt
 modify s atr = fmap (\x -> modify x atr) s
 
instance (Atributable a) => Atributable (Diagram a) where
 info d = case d of
                C a -> info a
                F a -> info a
                N a -> info a
                L a _ -> a
                B a _ -> a
                P a _ -> a
   
 modify d atr = case d of
                        C s -> C $ fmap (\x -> modify x atr) s
                        F s -> F $ fmap (\x -> modify x atr) s
                        N s -> N $ fmap (\x -> modify x atr) s
                        L a s -> L atr s
                        B a s -> B atr s
                        P a s -> P atr s

-- show instances

instance (Show a) => Show (Cat a) where
   show (Cat k) = "("++ (show k) ++ ")"

instance (Show a) => Show (Fc a) where
   show (Fc k s t) = (show s) ++ "--" ++ (show k) ++ "-->" ++ (show t)


instance (Show a) => Show (Line a) where
   show (Line [] e) = "@Error: Linea sin final. Sobran " ++ show e
   show (Line [t] []) = show t
   show (Line (t:e) []) = show t ++ " @Error:Cats sobrantes " ++ show e
   show (Line (s:cs) (a:as)) = (show s) ++ "--" ++ (show a) ++ "-->" ++ show (Line cs as)

instance (Show a) => Show (Nt a) where
   show (Nt k s t) = (show s)++"\n ||\n" ++ (show k)++ "\n ||\n\\ /\n" ++ (show t)

instance (Show a) => Show (Band a) where
   show (Band nts) = (intercalate ";" ( map (show . sourceNt) nts) ) ++ "\n ||\n" ++
                                       (intercalate ";" ( map (show . keyNt) nts) ) ++  "\n ||\n\\ /\n" ++
                                       (intercalate ";" ( map (show . targetNt) nts) )
                                                         

instance (Show a) => Show (Plane a) where
   show (Plane  bans) = intercalate "\n" $ map show bans

showD d = "Diagrama de tipo " ++ k ++ "\n" where
                                                  k = whichKind d

instance (Show a) => Show (Diagram a) where
  show (C c) = showD (C c) ++ (show c)
  show (F c) = showD (F c) ++ (show c)
  show (N c) = showD (N c) ++ (show c)
  show (L atr l) = show atr ++ "\n" ++ (showD (L atr l)) ++ (show l)
  show (B atr l) = show atr ++ "\n" ++ (showD (B atr l)) ++ (show l)
  show (P atr l) = show atr ++ "\n" ++ (showD (P atr l)) ++ (show l)
