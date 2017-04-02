module Reggie.Parser where

import Prelude hiding (read)
import Control.Applicative
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

data Parser i o = Parser (Maybe o) (IntMap (i -> Parser i o))

instance Show o =>
         Show (Parser i o) where
  show (Parser a t) = "Accept " ++ (show a) ++
                      " with transitions to " ++ (show (IntMap.keys t))

instance Monoid (Parser i o) where
    mempty = empty
    mappend = (<|>)

instance Functor (Parser i) where
    fmap f (Parser aa at) = Parser (fmap f aa) (fmap (\t i -> fmap f (t i)) at)

instance Applicative (Parser i) where
    pure o = Parser (pure o) mempty
    (Parser fa ft) <*> as@(Parser aa at) =
        Parser (fa <*> aa)
               (foldr (\f bs -> mappend bs (fmap (\t i -> fmap f (t i)) at))
                      (fmap (\t i -> (t i) <*> as) ft)
                      fa)

instance Alternative (Parser i) where
    empty = Parser empty mempty
    (Parser aa at) <|> (Parser ba bt) = Parser (aa <|> ba) (mappend at bt)

instance Monad (Parser i) where
    (Parser aa at) >>= f =
        foldr (\a as -> mappend as (f a))
              (Parser empty (fmap (\t i -> (t i) >>= f) at))
              aa

filterMap :: Int -> (i -> Maybe o) -> Parser i o
filterMap a f = Parser empty (IntMap.singleton a (\i -> foldMap pure (f i)))

read :: Parser i o -> i -> Parser i o
read (Parser _ ts) i = foldMap (\t -> t i) ts

rep :: (a -> b -> b) -> b -> Parser i a -> Parser i b
rep f acc a@(Parser _ at) =
    Parser (Just acc) (fmap (\t i -> (t i) >>= (\o -> rep f (f o acc) a)) at)
