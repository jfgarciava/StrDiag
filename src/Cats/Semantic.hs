module Cats.Semantic
  (--Types
  Semantic (..)
  -- functions
  , mkSemantic
  -- elements
  , boolId
  , latexLabel
  , latexMap
  ) where


import Cats.Types
import Cats.Atrib
import Cats.Gen

import Data.List

-- Una semantica 2-categorica permite interpretar un diagrama

data Semantic a b = Semantic {to:: Diagram a -> b}

mkSemantic::(Cat a -> b)->(Fc a -> b)->(Atrib -> Line a -> b)->(Nt a -> b)->(Atrib -> Band a -> b)->(Atrib -> Plane a -> b)-> Semantic a b
mkSemantic c f l n b p = Semantic (\d ->case d of
                                      C s -> c s
                                      F s -> f s
                                      N s -> n s
                                      L a s -> l a s
                                      B a s -> b a s
                                      P a s -> p a s
                                  )

-- Semantica para reconocer las identidades
boolId:: (IdEq a) => Semantic a Bool
boolId = mkSemantic (isId . keyCat) (isId . keyFc) (\atr l -> isIdL l) (isId . keyNt) (\atr b -> isIdB b) (\atr p -> isIdP p)


-- Semantica de label y Map en latex
----  Comandos Latex necesarios
---------- simbolos: \fComp, \nComp, \at
---------- 1-arg: \id, \Id, \mc
---------- 2-arg: \obj, \applyNt
---------- 3-arg: \fc, \morf, \nt

latexLabel:: Semantic (Atrib) (Maybe String) ---- asFormula
latexLabel = mkSemantic catToLatex fcToLatexLabel lineToLatexLabel ntToLatexLabel bandToLatexLabel planeToLatexLabel


catToLatex = Just . getLabel

fcToLatexLabel = Just . getLabel

lineToLatexLabel _ l
          | valid l = Just $ mkLineFormula  [(getLabel f, isId f) | f<-fcsAtr l]
          | otherwise = Nothing

ntToLatexLabel n
         | valid n = Just $ getLabel n
         | otherwise = Nothing

bandToLatexLabel _ ban
          | valid ban = let ls = contentB ban; as = map keyNt ls;
                            ss = map sourceNt ls; ts = map targetNt ls;
                            iss = map isIdL ss; its = map isIdL ts
                         in do ns <- Just $ [(getLabel a, isId a) | a <- as]
                               lss <- sequence $ map (lineToLatexLabel noAtr) ss
                               lts <- sequence $ map (lineToLatexLabel noAtr) ts
                               liss <- Just $ zip lss iss
                               lits <- Just $ zip lts its;
                               return $ mkBandFormula ns liss lits
          | otherwise = Nothing

planeToLatexLabel _ pla 
         | valid pla = let bans = contentP pla; ibans = map isIdB bans;
                       in do lbans <- sequence $ map (bandToLatexLabel noAtr) bans
                             libans <- Just $ zip lbans ibans
                             return $ mkPlaneFormula libans
         | otherwise = Nothing


latexMap:: Semantic (Atrib) (Maybe String) ---- asMap
latexMap = mkSemantic catToLatex fcToLatexMap lineToLatexMap ntToLatexMap bandToLatexMap planeToLatexMap


fcToLatexMap f = let s = source f; t= target f;
                 in do a <- catToLatex s
                       b <- catToLatex t
                       label <- fcToLatexLabel f
                       return $ mkFcMap (s == catTerm) label a b

lineToLatexMap atr l =let s = source l; t= target l; -- asMap = getCD asMapCD atr
                      in do  a <- catToLatex s
                             b <- catToLatex t
                             label <- lineToLatexLabel atr l
                             return $ mkFcMap (s == catTerm) label a b

ntToLatexMap (Nt atr s t)  = do a <- lineToLatexLabel noAtr s
                                b <- lineToLatexLabel noAtr t
                                label <- ntToLatexLabel (Nt atr s t)
                                return $ mkNtMap ((source s) == catTerm) label a b

bandToLatexMap atr ban  = let s = source ban; t= target ban;
                          in do a <- lineToLatexLabel noAtr s
                                b <- lineToLatexLabel noAtr s
                                label <- bandToLatexLabel atr ban
                                return $ mkNtMap ((source s) == catTerm) label a b

planeToLatexMap atr pla = let s = source pla; t= target pla;
                          in do a <- lineToLatexLabel noAtr s
                                b <- lineToLatexLabel noAtr s
                                label <- planeToLatexLabel atr pla
                                return $ mkNtMap ((source s) == catTerm) label a b

---- Construcción de formulas

mkLineFormula :: [(String, Bool)] -> String
mkLineFormula [(f,_)] = f -- las id se dibujan cuando estan solas
mkLineFormula ls = intercalate " \\fComp " $ reverse  [f | (f,isId) <-ls, not (isId)]

preComp (n,isIdn) (f,isIdf) = (if isIdf then n else concat ["\\applyNt{",n ,"}{", f, "}"],  isIdn)
posComp (f,isIdf) (n,isIdn) = (if isIdf then n else f ++ " \\at " ++ n, isIdn)


mkBandFormula:: [(String, Bool)] -> [(String, Bool)] -> [(String, Bool)] -> String
mkBandFormula [(n,_)] _ _ = n
mkBandFormula ns ss ts  = mkPlaneFormula $ zipWith posComp sS $ zipWith preComp ns ts  where
                                                                   sS = init $ [ (mkLineFormula ls, and [isId | (_ , isId) <-ls] ) | ls <- inits ss]
                                                                   tS = tail $ [ (mkLineFormula ls, and [isId | (_ , isId) <-ls] ) | ls <- tails ts]

mkPlaneFormula :: [(String,Bool) ] -> String
mkPlaneFormula [(f,_)] = f
mkPlaneFormula ls = intercalate " \\nComp " $ reverse  [f | (f,isId) <-ls, not (isId)]

---- Construcción de mapas

mkFcMap :: Bool -> String -> String -> String -> String
mkFcMap sourceIsCatTerm f sf tf = if sourceIsCatTerm
                                  then concat ["\\obj{",f,"}{", tf , "}"]
                                  else concat ["\\fc{",f,"}{", sf ,"}{",tf ,"}"]

mkNtMap :: Bool -> String -> String -> String -> String
mkNtMap sourceIsCatTerm f sf tf = if sourceIsCatTerm
                                  then concat ["\\morf{",f,"}{", sf , "}{", tf, "}"]
                                  else concat ["\\nt{",f,"}{", sf ,"}{",tf ,"}"]

