module Cats.Gr2 where

import Data.List

import Cats.Types
import Cats.Atrib
import Cats.Semantic2


--- Semantica de 2-Grafos (nodos, flechas horizontales y  flechas verticales) ubicados en un plano 2d

--- Definicion de tipos
type Coord = (Int,Int)
type Box = (Coord,Coord)

data Node = Node {at::Coord, keyNode::Atrib} -- corresponde a una Cat
                                 deriving (Eq)
data HEdge = HEdge {from::Coord, to::Coord, center::Coord, keyHEdge::Atrib} -- corresponde a un Fc
                                             deriving (Eq) -- el centro puede variar para doblarse (i.e Bending)
data VEdge = VEdge {fromL::Coord, toL::Coord, fromC::Coord, toC::Coord, keyVEdge::Atrib} --Corresponde a una Nt -- fromL,toL Line -- fromC,toC Cat 
                                            deriving (Eq)-- es como una cruz {sur:fromL, norte: toL, este: fromC, oeste: toC} 

data Gr2 = Gr2 {nodes::[Node], h_edges::[HEdge], v_edges::[VEdge], box::Box, globalAtr::Atrib} -- corresponde a un Diag
                                                                                deriving (Eq)

---- Manejo del encuadre

zero = ((0,0),(0,0))::Box
width ((a,_),(b,_)) = b-a
heigth ((_,a),(_,b)) = b-a

reSizeToFit:: Coord -> Box -> Box
reSizeToFit (x,y) ((minx,miny),(maxx,maxy)) = ((min minx x, min miny y),(max maxx x, max maxy y))

fitBox:: Gr2 -> Gr2
fitBox (Gr2 ns hs vs bx atr) = (Gr2 ns hs vs newBox atr) where
                                                     newBox = foldr reSizeToFit bx ls
                                                     ls = lsn ++ lsc
                                                     lsn = [at n | n<-ns]
                                                     lsc = [center h | h<-hs ]
--- show instances

instance Show Node where
  show (Node pos atr) = show atr ++ "@" ++ show pos ++"\n"

instance Show HEdge where
  show (HEdge s t c atr) = show atr ++ ":" ++ show s ++ "--"++ show c ++"->"++ show t ++"\n"

instance Show VEdge where
  show (VEdge sL tL _ _  atr) = show atr ++ ":" ++ show sL ++ "==>"++ show tL ++"\n"

instance Show Gr2 where
  show (Gr2 ns hs vs bx atr) = "2-Grafo "++ show atr ++ " enmarcado entre  " ++ show bx ++ ".\n"++
                                          "Nodos:\n" ++ (concat $ map show ns) ++
                                          "Aristas:\n" ++ (concat $ map show hs) ++
                                          "Cruces:\n" ++ (concat $ map show vs)
-- Definicion de la 2-semantica 

---- grafo nulo 
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

addVEdge:: Coord -> Coord -> Coord -> Coord -> Gr2 -> Gr2
addVEdge sL tL sC tC  (Gr2 ns hs vs bx atr) = Gr2 ns hs ((VEdge sL tL sC tC atr):vs)  newBx noAtr where
                                                                                      newBx = foldr reSizeToFit bx [sL, tL, sC, tC]

simplify :: Gr2 -> Gr2
simplify (Gr2 ns hs vs bx atr) = (Gr2 (nub ns) (nub hs) (nub vs) bx atr) -- nub removes duplicates 

append:: Gr2 -> Gr2 -> Gr2
append (Gr2 ns hs vs bx atr) (Gr2 ns1 hs1 vs1 (a,b) _) = simplify $ Gr2 (ns ++ ns1) (hs ++ hs1) (vs ++ vs1) newBx atr where
                                                                                       newBx = foldr reSizeToFit bx [a,b]


transform :: (Coord -> Coord) -> Gr2 -> Gr2 -- Aplica fun a todas las coordenadas, 
transform fun (Gr2 ns hs vs _ atr) = fitBox (Gr2 nst hst vst (o,o) atr) where
                                                           nst = map (\(Node pos atr ) -> (Node (fun pos) atr) ) ns
                                                           hst = map (\(HEdge s t c atr)->(HEdge (fun s) (fun t) (fun c) atr)) hs
                                                           vst = map (\(VEdge sL tL sC tC atr)->(VEdge (fun sL) (fun tL) (fun sC) (fun tC) atr)) vs
                                                           o = fun (0,0) -- nuevo centro para enmarcar el grafo

translate n gr = transform (\(a,b) -> (a+n,b)) gr -- Horizontal

h_edgeComp:: Coord -> (Gr2,Gr2,Gr2) -> Gr2 -- -> fComp
h_edgeComp (x,y) (grAtr, grS, grT) = let gr = foldl1 append [grAtr,translate (-1)  grS,translate (1) grT]
                                     in addHEdge (x-1,y) (x+1,y) (x,y) gr

centralT:: Gr2 -> [Coord]--- elige elementos centrales para la banda final "target" de un diagrama
centralT (Gr2 _ _ [] ((x1,_),(x2,_)) _) = [(x1,0),(x2,0)]
centralT (Gr2 _ _ vs ((x1,_),(_,y2)) _) = (x1,0):[toC v | v<- vs, (snd $ toL v) == y2 ]

centralS:: Gr2 -> [Coord]--- elige elementos centrales para la banda inicial  "source" de un diagrama
centralS (Gr2 _ _ [] ((x1,_),(x2,_)) _) = [(x1,0),(x2,0)]
centralS (Gr2 _ _ vs ((x1,y1),(_,_)) _) = (x1,0):[toC v | v<- vs, (snd $ fromL v) == y1 ]


commonScale a b = (div m 2, div m a, div m b) where m = lcm a b --- las escalas siempre son pares

addFixCoord:: (Coord -> Coord) -> Coord  -> Coord -> Coord
addFixCoord fun fix pos = if pos == fix then fix else  fun pos

bendUp n gr = transform (foldl addFixCoord (\(a,b) -> (a,b+n)) (centralS gr)) gr --Deja fijas las coordendas centrales al inicio del grafo

bendDown n gr = transform (foldl addFixCoord (\(a,b) -> (a,b-n)) (centralT gr)) gr --Deja fijas las coordendas centrales al final del grafo

reCenter gr = translate (-m) $ bendUp (-n) gr where
                                  m = div (x1 + x2) 2
                                  n = div (y1 + y2) 2
                                  ((x1,y1),(x2,y2)) = box gr 

v_edgeComp:: Coord -> (Gr2,Gr2,Gr2) -> Gr2 -- -> nComp
v_edgeComp (x,y) (grAtr, grS, grT) = let (m,p,q) = commonScale (width $ box grS) (width $ box grT)
                                         trS = bendDown (1)  . (transform (\(a,b) -> (p * a, b))) . reCenter
                                         trT = bendUp  (1)  . (transform (\(a,b) -> (q * a, b))) . reCenter
                                         gr = foldl1 append [grAtr, trS grS, trT grT]
                                     in addVEdge (x,y-1) (x,y+1) (x-m,y) (x+m,y) gr

appendH gr1 gr2 = append gr1 gr2t where
                              gr2t= translate (-k+h) gr2
                              (_,(h,_)) = box gr1
                              ((k,_),_) = box gr2

h_append :: Coord -> Gr2 -> (Gr2, Gr2, Gr2) -> Gr2
h_append (x,y) gr (grAtr, grS , grT) =  appendH gr grn where
                                              grn = h_edgeComp (x,y) (grAtr, grS , grT)                                              

h_concat:: Coord -> [(Gr2, Gr2, Gr2)] -> Gr2  -- ->lComp
h_concat pos ls = foldl (h_append pos) grNull ls  

hv_append:: Coord -> Gr2 -> (Gr2, Gr2, Gr2) -> Gr2
hv_append (x,y) gr (grAtr, grS , grT) = appendH gr grn where
                                              grn = v_edgeComp (x,y) (grAtr, grS , grT)

hv_concat:: Coord -> [(Gr2, Gr2, Gr2)] -> Gr2 -- ->bComp
hv_concat pos ls = foldl (hv_append pos) grNull ls 

v_append:: Coord -> Gr2 -> Gr2 -> Gr2
v_append pos grS grT = append (trS grS) (trT grT) where
                                               trT = bendUp (-h)
                                               trS = bendDown (k)
                                               ((_,h),(_,_)) = box grT
                                               ((_,_),(_,k)) = box grS


v_concat:: Coord -> ( Gr2, [Gr2]) -> Gr2 -- ->dComp
v_concat pos (grAtr , ls) = foldl (v_append pos) grAtr ls


gr2Sem = Sem2 getGr2 id id id id id addNode h_edgeComp v_edgeComp h_concat hv_concat v_concat

toGr2:: (Meaningfull b) => b -> Gr2
toGr2 = meaning gr2Sem (0,0)

--To Do : Eliminar ids
