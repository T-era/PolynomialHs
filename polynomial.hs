module Polynomial (Shk(Shk), (<<-), (<<:)) where

import Term
import Data.List

newtype Shk k = Shk [Kou k]

instance (Num k, Eq k, Enum k, Ord k) => Num (Shk k) where
  fromInteger x = Shk [fromInteger x]
  negate (Shk ks) = Shk (map negate ks)
  abs _ = error "can't"
  signum _ = error "can't"
  (Shk kl1) * (Shk kl2) = Shk (unify [k1 * k2 | k1 <- kl1, k2 <- kl2])
  (Shk kl1) + (Shk kl2) = Shk (unify (kl1 ++ kl2))


instance (Num k, Eq k, Enum k, Ord k, Show k) => Show (Shk k) where
  show (Shk ks) = result
    where
      result
        | showNaked ks == "" = "0"
        | otherwise          = showNaked ks
      showNaked [] = []
      showNaked (k:ks) = (show k) ++ (showNaked ks)

instance (Eq k) => Eq (Shk k) where
  Shk k1 == Shk k2 = k1 == k2

unify :: (Enum k, Eq k, Num k, Ord k) => [Kou k] -> [Kou k]
unify [] = []
unify (a:ls) = sort (_unify a (unify ls))
  where
    _unify a [] = [a]
    --_unify 0 (b:bs) = _unify b bs
    _unify a (b:bs)
      | canSum a b = (a+b:bs)
      | otherwise  = (b:_unify a bs)

(<<-) :: (Enum k, Eq k, Num k, Ord k) => (Shk k) -> [k] -> (Shk k)
(Shk kl) <<- x = Shk $ unify ans
  where
    ans = map (\k -> (k <<-- x)) kl

(<<:) :: (Enum d, Eq d, Num d, Ord d, Integral d, Num k, Eq k) => (Shk d) -> [k] -> k
(Shk kl) <<: x = ans
  where
    ans = foldr (+) 0 (map (\k -> (k <<:: x)) kl)
