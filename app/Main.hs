module Main where

import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B 

import Lib (library)
--import Cats.Semantic2 (toLatexMap)



replace :: String -> String -> B.ByteString -> B.ByteString

replace pat str file = let (beg,temp) = B.break (== '%') file
                       in if temp == B.empty then beg
                           else case B.stripPrefix (B.pack ("%"++ pat)) temp of
                                          (Just end) -> B.concat [beg, B.pack str, end]
                                          Nothing -> B.concat [beg, B.pack "%", (replace pat str (B.drop 1 temp))]

main :: IO ()
main = do
  (name:item:_) <- getArgs
  template <- B.readFile "./tex/template.tex"
  fileName <- return $  "./tex/" ++ name ++ ".tex"
  case lookup item library of
     (Just diag) -> B.writeFile fileName content where
                         content =  replace "(Map)" (show diag) template
     Nothing -> putStrLn "Item no encontrado."
  
