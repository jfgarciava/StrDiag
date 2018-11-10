{-# LANGUAGE FlexibleInstances #-}

module Cats.Semantic where


import Cats.Types
import Cats.Atrib

import Data.List
import qualified Data.Text as T



--- Semantica de formulas latex
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
                           if (s == catTerm)
                               then return $ concat ["\\obj{",label,"}{", b , "}"]
                               else return $ concat ["\\fc{",label,"}{", a,"}{", b ,"}"]



instance ToLatex [Fc] where
 tolatex [fc] = Just $ getLabel $ keyFc fc
 tolatex fcs            
          | checkList fcs = Just $ (concat . reverse) $ map (drawLabel . keyFc) fcs
          | otherwise = Nothing

instance ToLatex Line where
 tolatex lin =  do label <- tolatex $ contentL lin
                   a <- tolatex $ source lin
                   b <- tolatex $ target lin
                   if (source lin == catTerm)
                      then return $ concat ["\\obj{",label,"}{", b , "}"]
                      else return $ concat ["\\fc{",label,"}{", a,"}{", b ,"}"]

instance ToLatex Nt where
 tolatex (Nt atr s t)
         |valid  (Nt atr s t) = do a <- tolatex s
                                   b <- tolatex t
                                   label <- Just $ getLabel atr
                                   if ((source $ Line s) == catTerm)
                                       then return $ concat ["\\morf{",label,"}{",a,"}{", b, "}"]
                                       else return $ concat ["\\nt{",label,"}{",a,"}{",b ,"}"]
         |otherwise    = Nothing

isIdLabel:: Latex -> Bool
isIdLabel s = (take 4 s) == "\\id{"


idComp:: Latex -> Latex-> Latex
idComp a b
  | isIdLabel a = b
  | isIdLabel b = a 
  | otherwise = concat [a, b]


preComp:: Latex -> Latex -> Latex
preComp "" _ = ""
preComp a "" = a 
preComp a  b = concat [a, "_{", b, "}"]

posComp:: Latex -> Latex -> Latex
posComp _ "" = ""
posComp a  b = concat [a,b]


instance ToLatex [Nt] where ----la más importante y difícil
 tolatex [nt] 
          | valid nt = Just $ getLabel $ keyNt nt                  
 tolatex nts
          | checkList nts = do s <- sequence $ map (tolatex . sourceNt) nts
                               t <- sequence $ map (tolatex . targetNt) nts
                               let s1 = init $ scanl (idComp) "" (reverse s)   
                                   t1 =tail $ scanr (idComp) "" (reverse t)
                                   l = reverse$ map (drawLabel . keyNt)  nts
                                   q = zipWith preComp l t1
                                in return $ concat $ zipWith posComp s1 q
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


