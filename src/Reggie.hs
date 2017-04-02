module Reggie (filterMap, lit, parser, pred, read, rep) where

import Prelude       hiding (map, pred, read)
import Reggie.Parser hiding (filterMap, rep)
import Reggie.Rule

lit :: Eq a => a -> Rule a a
lit a = pred (a ==)

pred :: (a -> Bool) -> Rule a a
pred p = filterMap (\a -> if (p a) then Just a else Nothing)
