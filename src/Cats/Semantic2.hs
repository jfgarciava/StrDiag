{-# LANGUAGE FlexibleInstances #-}

module Cats.Semantic2 where

import Data.List
import Cats.Gen

import Cats.Types
import Cats.Atrib

-- Una semantica 2-categorica cuenta con composiciones lineal (fc), horizontal (nt) y vertical (nt)
-- mod es un modificador, contiene todos los posibles modos de una semantica, la semanta incluye información sobre como cambiar de modo
-- No veo necesidad de  mod ->

data Sem2  d = Sem2 { loadAtr :: Atrib -> d,
                         --- modificadores
                       --  atrMod:: mod -> mod,
                       --  catSMod:: mod -> mod,
                       --  catTMod:: mod -> mod,
                       --  fcSMod:: mod -> mod,
                       --  fcTMod:: mod -> mod,
                         --- computadores
                         cComp:: d -> d,
                         fComp:: (d,d,d) -> d,
                         nComp:: (d,d,d) -> d,
                         lComp:: [(d,d,d)] -> d,
                         bComp:: [(d,d,d)] -> d,
                         dComp:: (d,[d]) -> d }

--- Instancias

class Meaningfull b where
  meaning:: Sem2  d -> b -> d 

instance Meaningfull Atrib where
  meaning sem atr = loadAtr sem atr

instance Meaningfull Cat where
  meaning sem (Cat atr) = cComp sem  $ meaning sem atr

instance Meaningfull Fc where
  meaning sem (Fc atr s t) = fComp sem $ ( geta atr, gets s, gett t) where
                                                                  geta = meaning sem 
                                                                  gets = meaning sem 
                                                                  gett = meaning sem 

instance Meaningfull Line where
  meaning sem (Line fs) = lComp sem $ [ (geta (info f), gets $ source f , gett $ target f) | f<-fs] where
                                                                                       geta = meaning sem 
                                                                                       gets = meaning sem 
                                                                                       gett = meaning sem 

instance Meaningfull Nt where
  meaning sem (Nt atr s t) = nComp sem  $  (geta atr, gets $ Line s , gett $ Line t) where
                                                                                       geta = meaning sem 
                                                                                       gets = meaning sem 
                                                                                       gett = meaning sem 

instance Meaningfull Band where
  meaning sem (Band ns) = bComp sem $ [ (geta (info n), gets $ Line $ sourceNt n  , gett $ Line $ targetNt n) | n<-ns] where
                                                                                                                geta = meaning sem 
                                                                                                                gets = meaning sem 
                                                                                                                gett = meaning sem 

instance Meaningfull Diag where
  meaning sem (Diag atr bs) = dComp sem $ (geta atr , [ getb $ b | b<-bs] ) where
                                                                          geta = meaning sem 
                                                                          getb = meaning sem 



-------- Semantica de label en latex


data TexLabel = Label {asKey::String, asMap::String, asFormula::String}

semLabel:: Sem2 TexLabel
semLabel = Sem2 load catLabel fcLabel ntLabel lineLabel bandLabel diagLabel

-- loadAtr

load atr = Label (name atr) "" (getCD labelCD atr) 

--- cComp

catLabel (Label n _ f) = Label n id f where id = concat ["\\Id{", f, "}"] 

-- fComp

mkFcMap sn f sf tf = if (sn == "@TerminalCat@")
                       then concat ["\\obj{",f,"}{", tf , "}"]
                       else concat ["\\fc{",f,"}{", sf ,"}{",tf ,"}"]


fcLabel (Label n _ f ,Label sn _ sf,Label _ _ tf) = Label n m f where
                                                             m= mkFcMap sn f sf tf
                                                                  
isIdLabel (Label n _ _) = (take 3 n == "id@")                                                                  

-- lComp

mkLineFormula ls = intercalate " \\f_comp " $ reverse  [asFormula a | a <-ls, not (isIdLabel a)]

lineLabel:: [(TexLabel, TexLabel,TexLabel)] -> TexLabel
lineLabel [] = Label "@NULL@" "" ""
lineLabel [(a, s, t)] = fcLabel (a, s, t)
lineLabel ls = Label n m f where 
                        n = intercalate "%>" $ [asKey a | (a,_,_) <-ls]  
                        f = mkLineFormula  [a | (a,_,_) <-ls]
                        m = mkFcMap sn f sf tf   where
                                              (_,s,_) = head ls
                                              (_,_,t) = last ls
                                              sn = asKey s
                                              sf = asFormula s
                                              tf = asFormula t

-- nComp

mkNtMap sm f sf tf = if (take 5 sm == "\\obj{")
                       then concat ["\\morf{",f,"}{", sf , "}{", tf, "}"]
                       else concat ["\\nt{",f,"}{", sf ,"}{",tf ,"}"]


ntLabel (Label n _ f, Label _ sm sf, Label _ _ tf) =  Label n m f where
                                                               m= mkNtMap sm f sf tf
  
-- bComp

preComp n f = concat [n, "_{", f, "}"]
posComp f n = f ++ " \\at " ++ n


mkBandFormula:: [(TexLabel, TexLabel,TexLabel)] -> String
mkBandFormula ls =(\s-> "("++s++")") $ concat $ zipWith posComp ts qs  where
                     ns = [asFormula a | (a,_,_) <-ls]
                     ss = init $ map mkLineFormula $ inits  [ s |(_,s,_)<-ls]
                     ts = tail $ map mkLineFormula $ tails  [ t |(_,_,t)<-ls]
                     qs = zipWith preComp ns ss 

bandLabel:: [(TexLabel, TexLabel,TexLabel)] -> TexLabel
bandLabel [] = Label "@NULL@" "" ""
bandLabel [(a, s, t)] = ntLabel (a, s, t)
bandLabel ls =  Label n m f where
                           f = mkBandFormula ls
                           n = intercalate "%$" $ [asKey a | (a,_,_) <-ls]
                           m = mkNtMap sm f sf tf   where
                                              sf = mkLineFormula [ s |(_,s,_)<-ls]
                                              tf = mkLineFormula [t|(_,_,t) <- ls]
                                              sm = asMap $ head [ s |(_,s,_)<-ls]

-- dComp

mkDiagFormula ls = intercalate " \\n_comp " $ reverse  [asFormula a | a <-ls, not (isIdLabel a)]

diagLabel:: (TexLabel, [TexLabel]) -> TexLabel
diagLabel (Label n _ _ , ls) = Label n m f where
                                         f = mkDiagFormula ls
                                         m = mkNtMap sm f sf tf   where
                                              sf = asMap $ head  ls
                                              tf = asMap $ last  ls
                                              sm = asMap $ head  ls

toLatexMap:: (Meaningfull b) => b -> String 
toLatexMap = asMap . meaning semLabel
