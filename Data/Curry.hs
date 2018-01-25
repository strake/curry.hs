module Data.Curry where

data Uncurry f ab where Uncurry :: { reCurry :: f a b } -> Uncurry f '(a, b)

deriving instance Eq (f a b) => Eq (Uncurry f '(a, b))
deriving instance Ord (f a b) => Ord (Uncurry f '(a, b))
deriving instance Read (f a b) => Read (Uncurry f '(a, b))
deriving instance Show (f a b) => Show (Uncurry f '(a, b))

instance Bounded (f a b) => Bounded (Uncurry f '(a, b)) where
    minBound = Uncurry minBound
    maxBound = Uncurry maxBound

instance Semigroup (f a b) => Semigroup (Uncurry f '(a, b)) where
    Uncurry x <> Uncurry y = Uncurry (x <> y)

instance Monoid (f a b) => Monoid (Uncurry f '(a, b)) where
    mempty = Uncurry mempty
    Uncurry x `mappend` Uncurry y = Uncurry (x `mappend` y)

data Curry f a b where Curry :: { unCurry :: f '(a, b) } -> Curry f a b
