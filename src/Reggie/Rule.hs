module Reggie.Rule where

import           Prelude                  hiding (map, seq)
import           Control.Applicative
import           Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State
import           Reggie.Parser            (Parser)
import qualified Reggie.Parser            as Parser

newtype Rule i o = Rule (State Int (Parser i o))

instance Monoid (Rule i o) where
    mempty = empty
    mappend = (<|>)

instance Functor (Rule i) where
    fmap f (Rule ar) = Rule (fmap (fmap f) ar)

instance Applicative (Rule i) where
    pure o = Rule (return (pure o))
    (Rule fr) <*> (Rule ar) = Rule (do f <- fr
                                       a <- ar
                                       return (f <*> a))

instance Alternative (Rule i) where
    empty = Rule (return mempty)
    (Rule ar) <|> (Rule br) = Rule (do a <- ar
                                       b <- br
                                       return (a <|> b))

filterMap :: (i -> Maybe o) -> Rule i o
filterMap f = Rule (State.state (\a -> (Parser.filterMap a f, a + 1)))

parser :: Rule i o -> Parser i o
parser (Rule r) = State.evalState r 1

rep :: (a -> b -> b) -> b -> Rule i a -> Rule i b
rep f b (Rule ar) = Rule (fmap (Parser.rep f b) ar)
