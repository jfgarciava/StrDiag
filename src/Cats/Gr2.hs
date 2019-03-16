module Cats.Gr2 where

import Data.List

import Cats.Types
import Cats.Atrib
import Cats.Semantic

--- Definicion de tipos
type Coord = (Int,Int)
type Box = (Coord,Coord)
type Graph2 a = Diagram (Coord, a) 

--graph2:: (IdEq a) => Semantic a (Graph2 a)
--graph2= mkSemantic catToGr2 fcToGr2 lineToGr2 ntToGr2 bandToGr2 planeToGr2

catAt (x,y) (Cat k) = Cat ((x,y),k)
fcAt (x,y) f = Fc ((x+1,y),k) s t where
                               k= keyFc f
                               s= catAt (x,y) $ source f
                               t= catAt (x+2,y) $ target f

lineAt (x,y) (Line [] _) = Line [] []
lineAt (x,y) (Line [c] _) = Line [] []
lineAt (x,y) (Line (c:t:cs) (k:ks)) = (Line [catAt (x,y) c, catAt (x+2,y) t] [((x+1,y),k)]) <> (lineAt (x+2,y) (Line (t:cs) ks))

--ntAt (x,y) (Nt k s t) =
