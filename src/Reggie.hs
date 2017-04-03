module Reggie (lit, Parser, parser, pred, read, rep, transition) where

import Control.Applicative
import Prelude hiding (pred, read)
import Reggie.Parser (Parser, read)
import Reggie.Rule

lit :: (Eq a, Alternative m) => a -> Rule a m a
lit a = pred (a ==)

pred :: Alternative m => (a -> Bool) -> Rule a m a
pred p = transition (\a -> if (p a) then pure a else empty)
