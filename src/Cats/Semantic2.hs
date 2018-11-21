{-# LANGUAGE FlexibleInstances #-}

module Cats.Semantic2 where

import Data.List

import Cats.Types
import Cats.Atrib

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
  meaning sem m (Line fs) = lComp sem m $ [ (geta (info f), gets $ source f , gett $ target f) | f<-fs] where
                                                                                       geta = meaning sem (atrMod sem m)
                                                                                       gets = meaning sem (catSMod sem m)
                                                                                       gett = meaning sem (catTMod sem m)

instance Meaningfull Nt where
  meaning sem m (Nt atr s t) = nComp sem m  $  (geta atr, gets $ Line s , gett $ Line t) where
                                                                                       geta = meaning sem (atrMod sem m)
                                                                                       gets = meaning sem (fcSMod sem m)
                                                                                       gett = meaning sem (fcTMod sem m)

instance Meaningfull Band where
  meaning sem m (Band ns) = bComp sem m $ [ (geta (info n), gets $ Line $ sourceNt n  , gett $ Line $ targetNt n) | n<-ns] where
                                                                                                                geta = meaning sem (atrMod sem m)
                                                                                                                gets = meaning sem (fcSMod sem m)
                                                                                                                gett = meaning sem (fcTMod sem m)

instance Meaningfull Diag where
  meaning sem m (Diag atr bs) = dComp sem m $ (geta atr , [ getb $ b | b<-bs] ) where
                                                                          geta = meaning sem (atrMod sem m)
                                                                          getb = meaning sem m



