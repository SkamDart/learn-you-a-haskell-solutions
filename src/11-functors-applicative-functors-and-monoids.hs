import           Control.Applicative
import           Data.Monoid

data List a = Empty | Value a (List a)
    deriving (Show)

-- Functor
instance Functor List where
  fmap f (Value a x) = Value (f a) (fmap f x)
  fmap f Empty       = Empty

-- Semigroup
-- <> is syntatic sugar for mappend
instance Semigroup (List a) where
    Empty       <> Empty    = Empty
    Empty       <> x        = x
    x           <> Empty    = x
    (Value a x) <> y        = (Value a (x <> y))

-- Monoid
instance Monoid (List a) where
  mempty = Empty

-- Applicative
instance Applicative List where
  pure a = (Value a Empty)
  Empty <*> _ = Empty
  (Value f _) <*> Empty = Empty

