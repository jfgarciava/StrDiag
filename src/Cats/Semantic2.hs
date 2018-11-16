{-# LANGUAGE FlexibleInstances #-}

module Cats.Semantic2 where

---import Data.Aeson
import Data.List

import Cats.Types
import Cats.Atrib
--import Cats.Semantic


-- Una semantica 2-categorica cuenta con composiciones lineal (fc), horizontal (nt) y vertical (nt)
-- mod es un modificador, contiene todos los posibles modos de una semantica

data Sem2 mod d = Sem2 { getD :: mod -> Atrib -> d,
                         fComp:: mod -> (d,d,d) -> d,
                         cComp:: mod -> d -> d,
                         nComp:: mod -> (d,d,d) -> d,
                         lComp:: mod -> [(d,d,d)] -> d,
                         bComp:: mod -> [(d,d,d)] -> d,
                         dComp:: mod -> (d,[d]) -> d }

--- Instancias

class Meaningfull b where
  meaning:: Sem2 mod d -> mod -> b -> d 

instance Meaningfull Atrib where
  meaning sem m atr = (getD sem) m atr

instance Meaningfull Cat where
  meaning sem m (Cat atr) = (cComp sem) m $ meaning sem m atr

instance Meaningfull Fc where
  meaning sem m (Fc atr s t) = (fComp sem) m $ ( geta atr, getc s, getc t) where
                                                                  geta = meaning sem m
                                                                  getc = meaning sem m

instance Meaningfull Line where
  meaning sem m (Line fs) = (lComp sem) m $ [ (getf f, getc $ source f , getc $ target f) | f<-fs] where
                                                                                       getf = meaning sem m
                                                                                       getc = meaning sem m

instance Meaningfull Nt where
  meaning sem m (Nt atr s t) = (nComp sem) m  $  (geta atr, getl $ Line s , getl $ Line t) where
                                                                                       geta = meaning sem m
                                                                                       getl = meaning sem m

instance Meaningfull Band where
  meaning sem m (Band ns) = (bComp sem) m $ [ (getn n, getl $ Line $ sourceNt n  , getl $ Line $ targetNt n) | n<-ns] where
                                                                                                                 getn = meaning sem m
                                                                                                                 getl = meaning sem m

instance Meaningfull Diag where
  meaning sem m (Diag atr bs) = (dComp sem) m $ (geta atr , [ getb $ b | b<-bs] ) where
                                                                          geta = meaning sem m
                                                                          getb = meaning sem m

--- Semantica de diagramas (nodos, flechas, dobleflecha) ubicados en un plano 2d

type Coord = (Int,Int)

data Node = Node {at::Coord, keyNode::Atrib} deriving (Eq, Show)
data Edge = Edge {from::Coord, to::Coord, center::Coord, keyEdge::Atrib} deriving (Eq, Show)
data DEdge = DEdge {desde::Coord, hacia::Coord, keyDEdge::Atrib} deriving (Eq, Show)

data Gr2 = Gr2 {nodes::[Node], edges::[Edge], dedges::[DEdge], size::Coord}

data Mod = NO|ED|DE deriving (Eq, Show)

getGR2 NO atr = Gr2 [Node (0,0) atr] [] [] (0,0)
getGR2 ED atr = Gr2 [] [Edge (0,0) (0,0) (0,0) atr] [] (0,0)
getGR2 DE atr = Gr2 [] [] [DEdge (0,0) (0,0) atr] (0,0)


--gr2Semantic = Sem2 
