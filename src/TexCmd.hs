module TexCmd where

data TexCmd = TexCmd {cmd::String, args::[String]}

instance Show TexCmd where
  show (TexCmd cmd args) = concat $ ["\\", cmd] ++ map (\x -> "{"++ x++ "}") args

texArgs:: String -> Maybe [String]
texArgs [] = Just []
texArgs s
      | (head s == '{') = do r <- texArgs $ tail s1
                             return ([arg] ++ r)  where
                                 (arg, s1) =   break (=='}') $ tail s
texArgs _  = Nothing



texCmdArgs:: String -> Maybe TexCmd
texCmdArgs s
         |(head s == '\\') =do r <- texArgs args
                               return (TexCmd cmd r)   where
                               (cmd,args) = break (=='{') $ tail s
                              

texCmdArgs _ = Nothing
