module Term(Kou(Kou), canSum, (<<--)) where

data Kou k = Kou { constant :: k, coefficient :: [k] }

instance (Num k, Eq k, Enum k, Ord k) => Num (Kou k) where
  fromInteger x = Kou (fromInteger x) []
  negate (Kou con coe) = Kou (negate con) coe
  abs (Kou con coe) = Kou (abs con) coe
  signum (Kou con _) = Kou (signum con) []
  (Kou con1 coe1) * (Kou con2 coe2) = Kou (con1 * con2) [a+b | (a, b) <- _zipLs coe1 coe2]
  (+) k1@(Kou con1 coe1) k2@(Kou con2 coe2)
    | canSum k1 k2 = Kou (con1 + con2) coe1

_zipLs al [] = map (\a -> (a,0)) al
_zipLs [] bl = map (\b -> (0,b)) bl
_zipLs (a:as) (b:bs) = ((a,b):_zipLs as bs)

instance (Num k, Eq k, Enum k, Ord k, Show k) => Show (Kou k) where
  show (Kou con coe)
    | con == 0  = ""
    | con > 0   = "+" ++ (showCon con) ++ showCoeff coe "abcdefg"
    | otherwise = (showCon con) ++ showCoeff coe "abcdefg"

instance (Eq k) => Eq (Kou k) where
  (==) (Kou con1 coe1) (Kou con2 coe2) = (con1 == con2) && (coe1 == coe2)

instance (Eq k, Ord k) => Ord (Kou k) where
  compare (Kou con1 coe1) (Kou con2 coe2) = compare coe1 coe2

canSum (Kou _ coe1) (Kou _ coe2) = coe1 == coe2

showCon 1 = ""
showCon (-1) = "-"
showCon x = show x
showCoeff [] _ = ""
showCoeff (c:cs) (a:as)
  | c == 0    = showCoeff cs as
  | c == 1    = [a] ++ (showCoeff cs as)
  | otherwise = [a] ++ "^" ++ (show c) ++ (showCoeff cs as)

(<<--) :: (Num k, Eq k) => (Kou k) -> [k] -> (Kou k)
(Kou con coe) <<-- ll = Kou (con * su) ncoe
  where
    (su, ncoe) = _apply coe ll
    _apply jl [] = (1, jl)
    _apply [] _ = (1, [])
    _apply (j:js) (e:es) = ((_pow e j) * nj, (0:ne))
      where
        (nj, ne) = _apply js es

_pow x y = __pow x y 1
  where
    __pow _ 0 acm = acm
    __pow x y acm = __pow x (y-1) (acm * x)
