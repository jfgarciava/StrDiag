module Cats.Semantic where


import Cats.Types
import Cats.Atrib
import Cats.Gen

import Data.List

-- Una semantica 2-categorica permite interpretar un diagrama

data Semantic a b = Semantic {interpret:: Diagram a -> b}

