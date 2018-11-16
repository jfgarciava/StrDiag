{-# LANGUAGE FlexibleInstances #-}

module Cats.Semantic2 where

---import Data.Aeson
import Data.List

import Cats.Types
import Cats.Atrib
--import Cats.Semantic


-- Una semantica 2-categorica cuenta con composiciones lineal (fc), horizontal (nt) y vertical (nt)
-- mod es un modificador, contiene todos los posibles modos de una semantica, la semanta incluye información sobre como cambiar de modo

data Sem2 mod d = Sem2 { loadAtr :: mod -> Atrib -> d,
                         --- modificadores
                         atrMod:: mod -> mod,
                         catSMod:: mod -> mod,
                         catTMod:: mod -> mod,
                         fcSMod:: mod -> mod,
                         fcTMod:: mod -> mod,
                         --- computadores
                         cComp:: mod -> d -> d,
                         fComp:: mod -> (d,d,d) -> d,
                         nComp:: mod -> (d,d,d) -> d,
                         lComp:: mod -> [(d,d,d)] -> d,
                         bComp:: mod -> [(d,d,d)] -> d,
                         dComp:: mod -> (d,[d]) -> d }

--- Instancias

class Meaningfull b where
  meaning:: Sem2 mod d -> mod -> b -> d 

instance Meaningfull Atrib where
  meaning sem m atr = loadAtr sem m atr

instance Meaningfull Cat where
  meaning sem m (Cat atr) = cComp sem m $ meaning sem (atrMod sem m) atr

instance Meaningfull Fc where
  meaning sem m (Fc atr s t) = fComp sem m $ ( geta atr, gets s, gett t) where
                                                                  geta = meaning sem (atrMod sem m)
                                                                  gets = meaning sem (catSMod sem m)
                                                                  gett = meaning sem (catTMod sem m)

instance Meaningfull Line where
  meaning sem m (Line fs) = lComp sem m $ [ (geta f, gets $ source f , gett $ target f) | f<-fs] where
                                                                                       geta = meaning sem (atrMod sem m)
                                                                                       gets = meaning sem (catSMod sem m)
                                                                                       gett = meaning sem (catTMod sem m)

instance Meaningfull Nt where
  meaning sem m (Nt atr s t) = nComp sem m  $  (geta atr, gets $ Line s , gett $ Line t) where
                                                                                       geta = meaning sem (atrMod sem m)
                                                                                       gets = meaning sem (fcSMod sem m)
                                                                                       gett = meaning sem (fcTMod sem m)

instance Meaningfull Band where
  meaning sem m (Band ns) = bComp sem m $ [ (geta n, gets $ Line $ sourceNt n  , gett $ Line $ targetNt n) | n<-ns] where
                                                                                                                geta = meaning sem (atrMod sem m)
                                                                                                                gets = meaning sem (fcSMod sem m)
                                                                                                                gett = meaning sem (fcTMod sem m)

instance Meaningfull Diag where
  meaning sem m (Diag atr bs) = dComp sem m $ (geta atr , [ getb $ b | b<-bs] ) where
                                                                          geta = meaning sem (atrMod sem m)
                                                                          getb = meaning sem m

--- Semantica de diagramas (nodos, flechas, dobleflecha) ubicados en un plano 2d

type Coord = (Int,Int)
type Box = (Coord,Coord)

zero = ((0,0),(0,0))::Box
width ((a,_),(b,_)) = b-a
heigth ((_,a),(_,b)) = b-a

reSizeToFit:: Coord -> Box -> Box
reSizeToFit (x,y) ((minx,miny),(maxx,maxy)) = ((min minx x, min miny y),(max maxx x, max maxy y))

data Node = Node {at::Coord, keyNode::Atrib}
                                 deriving (Eq, Show)
data HEdge = HEdge {from::Coord, to::Coord, center::Coord, keyHEdge::Atrib}
                                             deriving (Eq, Show) -- el centro puede variar para dar espacio a un VEdge(i.e Bending)
data VEdge = VEdge {desde::Coord, hacia::Coord, keyVEdge::Atrib}
                                            deriving (Eq, Show)

data Gr2 = Gr2 {nodes::[Node], h_edges::[HEdge], v_edges::[VEdge], box::Box, tempAtr::Atrib} 

noAtr = minAtr "@NULL@" 
grNull = Gr2 [] [] [] zero noAtr
-- d <- Gr2, mod <- Coord

getGr2:: Coord -> Atrib -> Gr2 -- -> loadAtr
getGr2 pos atr = Gr2 [] [] [] (pos,pos) atr

addNode:: Coord -> Gr2 -> Gr2  -- -> cComp
addNode pos (Gr2 ns hs vs bx atr) =  Gr2 ((Node pos atr):ns) hs vs (reSizeToFit pos bx) noAtr   

addHEdge:: Coord -> Coord -> Coord -> Gr2 -> Gr2
addHEdge s t c (Gr2 ns hs vs bx atr) = Gr2 ns ((HEdge s t c atr):hs) vs newBx noAtr where
                                                                                      newBx = foldr reSizeToFit bx [s,t,c]

addVEdge:: Coord -> Coord -> Gr2 -> Gr2
addVEdge s t (Gr2 ns hs vs bx atr) = Gr2 ns hs ((VEdge s t atr):vs)  newBx noAtr where
                                                                                      newBx = foldr reSizeToFit bx [s,t]

append :: Gr2 -> Gr2 -> Gr2
append (Gr2 ns hs vs bx atr) (Gr2 ns1 hs1 vs1 (a,b) _) = Gr2 (ns ++ ns1) (hs ++ hs1) (vs ++ vs1) newBx atr where
                                                                                       newBx = foldr reSizeToFit bx [a,b]

translate :: (Coord -> Coord) -> Gr2 -> Gr2 -- Aplica fun a todas las coordenadas, 
translate fun (Gr2 ns hs vs (p, q) atr) = (Gr2 nst hst vst bxt atr) where
                                                                 nst = map (\(Node pos atr ) -> (Node (fun pos) atr) ) ns
                                                                 hst = map (\(HEdge s t c atr)->(HEdge (fun s) (fun t) (fun c) atr)) hs
                                                                 vst = map (\(VEdge s t atr)->(VEdge (fun s) (fun t) atr)) vs
                                                                 bxt = (fun p, fun q) --fun debe ser convexa

fixEnds :: (Coord -> Coord) -> Coord -> Coord -> Coord -> Coord
fixEnds fun end1 end2 pos = if pos == end1 then end1 else (if pos == end2 then end2 else fun pos)

bendHEdge fun (x,y) = translate (fixEnds fun (x-1,y) (x+1,y))

h_edgeComp ::Coord -> (Gr2,Gr2,Gr2) -> Gr2 -- -> fComp
h_edgeComp (x,y) (grAtr, grS, grT) = let gr = foldl1 append [grAtr,translate (\(a,b) -> (a-1,b)) grS,translate (\(a,b) -> (a+1,b)) grT]
                                     in addHEdge (x-1,y) (x+1,y) (x,y) gr

v_edgeComp ::Coord -> (Gr2,Gr2,Gr2) -> Gr2 -- -> nComp
v_edgeComp (x,y) (grAtr, grS, grT) = let gr = foldl1 append [grAtr, bendHEdge  (\(a,b) -> (a,b+1)) (x,y)  grS, bendHEdge (\(a,b) -> (a,b-1)) (x,y)  grT]
                                     in addVEdge (x,y-1) (x,y+1) gr


h_Append :: Coord -> Gr2 -> (Gr2, Gr2, Gr2) -> Gr2
h_Append (x,y) gr (grAtr, grS , grT) = h_edgeComp (x,y) (grAtr, gra , grt) where
                                              grt = translate (\(a,b)-> (a+t,b)) grT
                                              grs = translate (\(a,b)-> (a+t,b)) grS
                                              gra = append grs gr
                                              t = width $ box gr

h_concat :: Coord -> [(Gr2, Gr2, Gr2)] -> Gr2
h_concat pos ls = foldl (h_Append pos) grNull ls  -- ->lComp

hv_Append :: Coord -> Gr2 -> (Gr2, Gr2, Gr2) -> Gr2
hv_Append (x,y) gr (grAtr, grS , grT) = v_edgeComp (x,y) (grAtr, gra , grt) where
                                              grt = translate (\(a,b)-> (a+t,b)) grT
                                              grs = translate (\(a,b)-> (a+t,b)) grS
                                              gra = append grs gr
                                              t = width $ box gr

hv_concat :: Coord -> [(Gr2, Gr2, Gr2)] -> Gr2
hv_concat pos ls = foldl (hv_Append pos) grNull ls -- ->bComp


-- ->dComp
