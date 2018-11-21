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


--- Construcción de formulas

mkLineFormula :: [(String, Bool)] -> String
mkLineFormula [(f,_)] = f -- las id se dibujan cuando estan solas
mkLineFormula ls = intercalate " \\f_comp " $ reverse  [f | (f,isId) <-ls, not (isId)]

preComp (n,isIdn) (f,isIdf) = (if isIdf then n else concat ["\\applyNt{",n ,"}{", f, "}"],  isIdn)
posComp (f,isIdf) (n,isIdn) = (if isIdf then n else f ++ " \\at " ++ n, isIdn)


mkBandFormula:: [(String, Bool)] -> [(String, Bool)] -> [(String, Bool)] -> String
mkBandFormula [(n,_)] _ _ = n
mkBandFormula ns ss ts  = mkDiagFormula $ zipWith posComp sS $ zipWith preComp ns ts  where                    
                                                                   sS = init $ [ (mkLineFormula ls, and [isId | (_ , isId) <-ls] ) | ls <- inits ss]
                                                                   tS = tail $ [ (mkLineFormula ls, and [isId | (_ , isId) <-ls] ) | ls <- tails ts]

mkDiagFormula :: [(String,Bool) ] -> String
mkDiagFormula [(f,_)] = f
mkDiagFormula ls = intercalate " \\n_comp " $ reverse  [f | (f,isId) <-ls, not (isId)]


--- construcción de mapas

mkFcMap :: Bool -> String -> String -> String -> String
mkFcMap sourceIsCatTerm f sf tf = if sourceIsCatTerm
                                  then concat ["\\obj{",f,"}{", tf , "}"]
                                  else concat ["\\fc{",f,"}{", sf ,"}{",tf ,"}"]
isObj s = (take 5 s == "\\obj{")

mkNtMap :: Bool -> String -> String -> String -> String
mkNtMap sourceIsCatTerm f sf tf = if sourceIsCatTerm
                                  then concat ["\\morf{",f,"}{", sf , "}{", tf, "}"]
                                  else concat ["\\nt{",f,"}{", sf ,"}{",tf ,"}"]

-- 2- Semantica de etiquetas latex en dos modos fórmula y mapa 

data TexLabel = Label {doDraw::Bool, asMap::String, asFormula::String}

semLabel:: Sem2 TexLabel
semLabel = Sem2 load catLabel fcLabel ntLabel lineLabel bandLabel diagLabel

-- loadAtr
load atr = Label (getCD drawCD atr) "" (getCD labelCD atr) 

--- cComp
catLabel (Label b _ f) = Label b id f where id = concat ["\\Id{", f, "}"] 

-- fComp
fcLabel (Label b _ f ,Label sb _ sf,Label _ _ tf) = Label b m f where
                                                             m= mkFcMap sb f sf tf
-- lComp
lineLabel:: [(TexLabel, TexLabel,TexLabel)] -> TexLabel
lineLabel [] = Label False "@Error: Empty line." "@Error: Empty line. "
lineLabel [(a, s, t)] = fcLabel (a, s, t)
lineLabel ls = Label b m f where 
                        b = or [doDraw a | (a,_,_) <-ls] 
                        f = mkLineFormula  [(asFormula a, not $ doDraw a) | (a,_,_) <-ls]
                        m = mkFcMap sb f sf tf   where
                                              (_,s,_) = head ls
                                              (_,_,t) = last ls
                                              sb = doDraw s
                                              sf = asFormula s
                                              tf = asFormula t

-- nComp
ntLabel (Label n _ f, Label _ sm sf, Label _ _ tf) =  Label n m f where
                                                               m= mkNtMap (isObj sm) f sf tf
  
-- bComp
bandLabel:: [(TexLabel, TexLabel,TexLabel)] -> TexLabel
bandLabel [] = Label False "@Error: Empty band." "@Error: Empty band. "
bandLabel [(a, s, t)] = ntLabel (a, s, t)
bandLabel ls =  Label b m f where
                           b = or [doDraw a | (a,_,_) <-ls] 
                           f = mkBandFormula ns ss ts where
                                                 ns = [(asFormula a, not $ doDraw a) | (a,_,_)<-ls]
                                                 ss = [(asFormula s, not $ doDraw s) | (_,s,_)<-ls]
                                                 ts = [(asFormula t, not $ doDraw t) | (_,_,t)<-ls]
                          
                           m = mkNtMap sb f sf tf   where
                                              sf = mkLineFormula [ (asFormula s, not $ doDraw s) |(_,s,_)<-ls]
                                              tf = mkLineFormula [ (asFormula t, not $ doDraw t) | (_,_,t) <- ls]
                                              sm = asMap $ head [ s |(_,s,_)<-ls]
                                              sb = isObj sm

-- dComp
diagLabel:: (TexLabel, [TexLabel]) -> TexLabel
diagLabel ( _ , [] ) = Label False "@Error: Empty diagram." "@Error: Empty diagram. "
diagLabel ( _ , [b]) = b
diagLabel ( _ , ls) = Label True m f where
                                         f = mkDiagFormula $ [ ( (\s-> "("++s++")") $ asFormula b , not $ doDraw b)| b<-ls]
                                         m = mkNtMap sb f sf tf   where
                                              sf = asFormula $ head  ls
                                              tf = asFormula $ last  ls
                                              sm = asMap $ head  ls
                                              sb = isObj sm

toLatexMap:: (Meaningfull b) => b -> String 
toLatexMap = asMap . meaning semLabel
