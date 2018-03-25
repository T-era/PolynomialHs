module Term(Kou(Kou), canSum, (<<--), (<<::)) where

data Kou n = Kou { constant :: n, coefficient :: [Integer] }

instance (Num n, Eq n, Enum n, Ord n) => Num (Kou n) where
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

instance (Num n, Eq n, Enum n, Ord n, Show n) => Show (Kou n) where
  show (Kou con coe)
    | con == 0  = ""
    | con > 0   = "+" ++ (showCon con (showCoeff coe ['a'..]))
    | otherwise = (showCon con (showCoeff coe ['a'..]))

instance (Eq n) => Eq (Kou n) where
  (==) (Kou con1 coe1) (Kou con2 coe2) = (con1 == con2) && (coe1 == coe2)

instance (Eq n, Ord n) => Ord (Kou n) where
  compare (Kou con1 coe1) (Kou con2 coe2) = compare coe1 coe2

canSum (Kou _ coe1) (Kou _ coe2) = coe1 == coe2

showCon 1 "" = "1"
showCon 1 a = a
showCon (-1) a = '-':a
showCon x a = show x ++ a
showCoeff [] _ = ""
showCoeff (c:cs) (a:as)
  | c == 0    = showCoeff cs as
  | c == 1    = [a] ++ (showCoeff cs as)
  | otherwise = [a] ++ "^" ++ (show c) ++ (showCoeff cs as)

(<<--) :: (Num n, Eq n) => (Kou n) -> [n] -> (Kou n)
(Kou con coe) <<-- ll = Kou (con * su) ncoe
  where
    (su, ncoe) = _apply coe ll
    _apply jl [] = (1, jl)
    _apply [] _ = (1, [])
    _apply (j:js) (e:es) = ((_pow e j) * nj, (0:ne))
      where
        (nj, ne) = _apply js es


(<<::) :: (Num d, Integral d, Eq d, Num k, Eq k) => (Kou d) -> [k] -> k
(Kou con coe) <<:: ll = (fromIntegral con) * (_apply0 coe ll)
  where
    _apply0 [] _ = 1
    _apply0 (0:js) [] = _apply0 js []
    _apply0 (_:js) [] = 0
    _apply0 (j:js) (e:es) = (_pow e j) * (_apply0 js es)

_pow x y = __pow x y 1
  where
    __pow _ 0 acm = acm
    __pow x y acm = __pow x (y-1) (acm * x)
