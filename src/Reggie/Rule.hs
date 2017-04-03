module Reggie.Rule where

import Control.Applicative
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State
import Reggie.Parser (Parser)
import qualified Reggie.Parser            as Parser

newtype Rule i m o = Rule (State Int (Parser i m o))

instance Functor m => Functor (Rule i m) where
    fmap f (Rule ar) = Rule (fmap (fmap f) ar)

instance (Applicative m, Foldable m) => Applicative (Rule i m) where
    pure o = Rule (return (pure o))
    (Rule fr) <*> (Rule ar) = Rule (do f <- fr
                                       a <- ar
                                       return (f <*> a))

instance (Alternative m, Foldable m) =>
         Alternative (Rule i m) where
    empty = Rule (return empty)
    (Rule ar) <|> (Rule br) = Rule (do a <- ar
                                       b <- br
                                       return (a <|> b))

transition :: Alternative m => (i -> m o) -> Rule i m o
transition f = Rule (State.state (\a -> (Parser.transition a f, a + 1)))

parser :: Rule i m o -> Parser i m o
parser (Rule r) = State.evalState r 1

rep :: (Alternative m, Foldable m) => (a -> b -> b) -> b -> Rule i m a -> Rule i m b
rep f b (Rule ar) = Rule (fmap (Parser.rep f b) ar)
