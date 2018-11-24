{-# LANGUAGE FlexibleInstances #-}

module Cats.Semantic where


import Cats.Types
import Cats.Atrib
import Cats.Gen

import Data.List
import qualified Data.Text as T



--- Semantica de formulas latex


-- Semantica de label en latex

----  Comandos Latex necesarios 
---------- simbolos: \fComp, \nComp, \at
---------- 1-arg: \id, \Id, \mc
---------- 2-arg: \obj, \applyNt
---------- 3-arg: \fc, \morf, \nt

---- Construcción de formulas

mkLineFormula :: [(String, Bool)] -> String
mkLineFormula [(f,_)] = f -- las id se dibujan cuando estan solas
mkLineFormula ls = intercalate " \\fComp " $ reverse  [f | (f,isId) <-ls, not (isId)]

preComp (n,isIdn) (f,isIdf) = (if isIdf then n else concat ["\\applyNt{",n ,"}{", f, "}"],  isIdn)
posComp (f,isIdf) (n,isIdn) = (if isIdf then n else f ++ " \\at " ++ n, isIdn)


mkBandFormula:: [(String, Bool)] -> [(String, Bool)] -> [(String, Bool)] -> String
mkBandFormula [(n,_)] _ _ = n
mkBandFormula ns ss ts  = mkDiagFormula $ zipWith posComp sS $ zipWith preComp ns ts  where                    
                                                                   sS = init $ [ (mkLineFormula ls, and [isId | (_ , isId) <-ls] ) | ls <- inits ss]
                                                                   tS = tail $ [ (mkLineFormula ls, and [isId | (_ , isId) <-ls] ) | ls <- tails ts]

mkDiagFormula :: [(String,Bool) ] -> String
mkDiagFormula [(f,_)] = f
mkDiagFormula ls = intercalate " \\nComp " $ reverse  [f | (f,isId) <-ls, not (isId)]

---- Construcción de mapas

mkFcMap :: Bool -> String -> String -> String -> String
mkFcMap sourceIsCatTerm f sf tf = if sourceIsCatTerm
                                  then concat ["\\obj{",f,"}{", tf , "}"]
                                  else concat ["\\fc{",f,"}{", sf ,"}{",tf ,"}"]

mkNtMap :: Bool -> String -> String -> String -> String
mkNtMap sourceIsCatTerm f sf tf = if sourceIsCatTerm
                                  then concat ["\\morf{",f,"}{", sf , "}{", tf, "}"]
                                  else concat ["\\nt{",f,"}{", sf ,"}{",tf ,"}"]



type Latex = String ---tal vez sea mejor escoger un tipo ,más rápido después por ejem T.Text

class ToLatex a where
 tolatex:: a -> Maybe Latex

seeLatex::(ToLatex a) => a -> IO ()
seeLatex x = case tolatex x of  Just s -> putStrLn s
                                Nothing -> putStrLn "Latex no generado: por favor, verifique que la expresión sea valida."

instance ToLatex Cat where
 tolatex (Cat atr) = Just $ getLabel atr

instance ToLatex Fc where
 tolatex (Fc atr s t) = do a <- tolatex s
                           b <- tolatex t
                           label <- Just $ getLabel atr
                           return $ mkFcMap (s == catTerm) label a b
--                           if (s == catTerm)
--                               then return $ concat ["\\obj{",label,"}{", b , "}"]
--                               else return $ concat ["\\fc{",label,"}{", a,"}{", b ,"}"]



instance ToLatex [Fc] where
 tolatex [fc] = Just $ getLabel $ keyFc fc
 tolatex fcs            
          | checkList fcs = Just $ mkLineFormula  [(getLabel f, not $ draw f) | f<-fcs]        ---- (concat . reverse) $ map (drawLabel . keyFc) fcs
          | otherwise = Nothing

instance ToLatex Line where
 tolatex lin =  do label <- tolatex $ contentL lin
                   a <- tolatex $ source lin
                   b <- tolatex $ target lin
                   return $ mkFcMap (source lin == catTerm) label a b
                --   if (source lin == catTerm)
                --      then return $ concat ["\\obj{",label,"}{", b , "}"]
                --      else return $ concat ["\\fc{",label,"}{", a,"}{", b ,"}"]

instance ToLatex Nt where
 tolatex (Nt atr s t)
         |valid  (Nt atr s t) = do a <- tolatex s
                                   b <- tolatex t
                                   label <- Just $ getLabel atr
                                   return $ mkNtMap ((source $ Line s) == catTerm) label a b
                             --      if ((source $ Line s) == catTerm)
                             --          then return $ concat ["\\morf{",label,"}{",a,"}{", b, "}"]
                             --          else return $ concat ["\\nt{",label,"}{",a,"}{",b ,"}"]
         |otherwise    = Nothing


instance ToLatex [Nt] where ----la más importante y difícil
 tolatex [nt] 
          | valid nt = Just $ getLabel $ keyNt nt                  
 tolatex nts
          | checkList nts = do s <- sequence $ map (tolatex . sourceNt) nts
                               t <- sequence $ map (tolatex . targetNt) nts
                               ns <- sequence $ [(tolatex n, draw n) | n <-  nts]
                               ss <- return $ zip s $ [and $ map draw (sourceNt n) | n <- nts]
                               ts <- return $ zip t $ [and $ map draw (targetNt n) | n <- nts]
                               return $ mkBandFormula ns ss ts
          | otherwise = Nothing

instance ToLatex Band where
 tolatex ban 
         |valid ban = do a <- tolatex $ contentL $ source ban
                         b <- tolatex $ contentL $ target ban
                         label <- tolatex $ contentB ban
                         if ((source $  head $ contentB ban) == catTerm)
                            then return $ concat ["\\morf{",label,"}{",a,"}{", b, "}"]
                            else return $ concat ["\\nt{",label,"}{",a,"}{",b ,"}"]
         |otherwise    = Nothing

parentesis:: Latex -> Latex
parentesis a = concat [ "(", a, ")"]


instance ToLatex [Band] where
 tolatex bans            
          | checkList bans = do a <- sequence  $ map (tolatex . contentB) bans
                                return $ (concat . reverse) $ map parentesis a
          | otherwise = Nothing

instance ToLatex Diag where
 tolatex d = do label <- tolatex $ contentD d
                a <- tolatex $ contentL $ source d
                b <- tolatex $ contentL $ target d
                if ((source $ source d) == catTerm)
                 then return $ concat ["\\morf{",label,"}{",a,"}{", b, "}"]
                 else return $ concat ["\\nt{",label,"}{",a,"}{",b ,"}"]


instance ToLatex [Diag] where
 tolatex [d] = tolatex d
 tolatex ds 
     | equatable ds = do names <- sequence  $ map (tolatex . contentD) ds
                         label <- Just $ intercalate " = " names
                         a <- tolatex $ contentL $ source $ head ds
                         b <- tolatex $ contentL $ target $ head ds
                         if ((source $ source $ head ds) == catTerm)
                           then return $ concat ["\\morf{",label,"}{",a,"}{", b, "}"]
                           else return $ concat ["\\nt{",label,"}{",a,"}{",b ,"}"]
     | otherwise = Nothing


