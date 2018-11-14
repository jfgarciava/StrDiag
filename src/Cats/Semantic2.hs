{-# LANGUAGE FlexibleInstances #-}

module Cats.Semantic2 where

import Data.Aeson
import Data.List

import Cats.Types
import Cats.Atrib
--import Cats.Semantic


-- Una semantica 2-categorica cuenta con composiciones lineal (fc), horizontal (nt) y vertical (nt)

class Sem2 d where
 lComp:: String -> [(d,d,d)] -> d
 hComp:: String -> [(d,d,d)] -> d
 vComp:: String -> [(d,d,d)] -> d 


--- Métodos

compL::(Sem2 d, FromJSON d) => Det d  -> Line -> d 
compL det (Line fs) = (lComp (tag det)) $ [ (get f, getc $ source f , getc $ target f) | f<-fs] where 
                                                                                                get = getCD det 
                                                                                                getc = getCD det

compB::(Sem2 d, FromJSON d) => Det d  -> Band -> d 
compB det (Band ns) = (hComp (tag det))$ [ ( get n, getl $ Line $ sourceNt n  , getl $ Line $ targetNt n ) | n<-ns] where 
                                                                                                             get = getCD det  
                                                                                                             getl = compL det 

compD::(Sem2 d, FromJSON d) => Det d  -> Diag -> d 
compD det (Diag _ bans) = (vComp (tag det)) $ [ ( get ban, getl $ source ban  , getl $ target ban ) | ban<-bans] where 
                                                                                                             get = compB det  
                                                                                                             getl = compL det
-- Semantica  "label"
isId:: String -> Bool
isId a = (take 4 a) == "\\Id{"

isid:: String -> Bool
isid a = (take 4 a) == "\\id{"

l2Comp:: String -> String -> String
l2Comp a b   
       | isId a = b
       | isId b = a
       | otherwise = b ++ " \\comp " ++ a 

h2Comp:: String -> String -> String
h2Comp a b 
       | isid a = b
       | isid b = a
       | otherwise = b ++ " \\star " ++ a 

preComp:: String -> String -> String
preComp n f 
       | isid n = n
       | isId f = n
       | otherwise = concat [n, "_{", f, "}"] 

posComp:: String -> String -> String
posComp f n 
       | isId f = n
       | isid n = n
       | otherwise = f ++ "\\at" ++ n


instance Sem2 String where
 lComp "label" [(l,_,_)] = l 
 lComp "label" ls = foldl (l2Comp) "\\Id{" $ [ l |(l,_,_)<-ls]
---intercalate " \\comp "  $ reverse [ l |(l,_,_)<-ls, not (isId l)] where
--                                                                    isId s = (take 4 s) == "\\Id{" 
 lComp _ ls = concat [ l |(l,_,_)<-ls]

 hComp "label" [(l,_,_)] = l 
 hComp "label" ls = let s1 = init $ scanl (l2Comp) "\\Id{"  [ s |(_,s,_)<-ls]   
                        t1 = init $ reverse $ scanl (l2Comp) "\\Id{"  [ t |(_,_,t)<-ls]
                        l1 = [ l |(l,_,_)<-ls]
                        q  = zipWith preComp l1 s1
                     in foldl (h2Comp) "\\id{" $ zipWith posComp t1 q

 hComp _ ls = concat [ l |(l,_,_)<-ls]

 vComp _ ls = concat [ l |(l,_,_)<-ls]


-- Semantica de diagramas de funtores tikzcd


