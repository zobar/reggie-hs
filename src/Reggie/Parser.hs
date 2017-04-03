module Reggie.Parser where

import Control.Applicative
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

data Parser i m o = Parser (m o) (IntMap (i -> Parser i m o))

instance Show (m o) =>
         Show (Parser i m o) where
  show (Parser a t) = "Accept " ++ (show a) ++
                      " with transitions to " ++ (show (IntMap.keys t))

instance Functor m =>
         Functor (Parser i m) where
    fmap f (Parser aa at) = Parser (fmap f aa) (fmap (\t i -> fmap f (t i)) at)

instance (Applicative m, Foldable m) =>
         Applicative (Parser i m) where
    pure o = Parser (pure o) mempty
    Parser fa ft <*> as@(Parser aa at) =
        Parser (fa <*> aa)
               (foldr (\f bs -> mappend bs (fmap (\t i -> fmap f (t i)) at))
                      (fmap (\t i -> (t i) <*> as) ft)
                      fa)

instance (Alternative m, Foldable m) =>
         Alternative (Parser i m) where
    empty = Parser empty mempty
    (Parser aa at) <|> (Parser ba bt) = Parser (aa <|> ba) (mappend at bt)

instance (Alternative m, Foldable m) =>
         Monad (Parser i m) where
    (Parser aa at) >>= f = foldr (\a as -> as <|> (f a))
                                 (Parser empty (fmap (\t i -> (t i) >>= f) at))
                                 aa

transition :: Alternative m =>
              Int -> (i -> m o) -> Parser i m o
transition a f = Parser empty (IntMap.singleton a (\i -> Parser (f i) mempty))

read :: (Alternative m, Foldable m) =>
        Parser i m o -> i -> Parser i m o
read (Parser _ ts) i = foldr (\f acc -> (f i) <|> acc) empty ts

rep :: (Alternative m, Foldable m) =>
       (a -> b -> b) -> b -> Parser i m a -> Parser i m b
rep f acc a@(Parser _ at) =
    Parser (pure acc) (fmap (\t i -> (t i) >>= (\o -> rep f (f o acc) a)) at)
