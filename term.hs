module Term(Kou(Kou), canSum, (<<--)) where

data Kou k = Kou { constant :: k, coefficient :: [k] }

instance (Num k, Eq k, Enum k, Ord k) => Num (Kou k) where
  fromInteger x = Kou (fromInteger x) []
  negate (Kou con coe) = Kou (negate con) coe
  abs (Kou con coe) = Kou (abs con) coe
  signum (Kou con _) = Kou (signum con) [0,0..]
  (Kou con1 coe1) * (Kou con2 coe2) = Kou (con1 * con2) [a+b | (a, b) <- zipWithFill 0 coe1 coe2]
  (+) k1@(Kou con1 coe1) k2@(Kou con2 coe2)
    | canSum k1 k2 = Kou (con1 + con2) coe1

instance (Num k, Eq k, Enum k, Ord k, Show k) => Show (Kou k) where
  show (Kou con coe)
    | con == 0  = ""
    | con > 0   = "+" ++ (showCon con) ++ showCoeff coe "abcdefg"
    | otherwise = (showCon con) ++ showCoeff coe "abcdefg"

instance (Eq k) => Eq (Kou k) where
  (==) (Kou con1 coe1) (Kou con2 coe2) = (con1 == con2) && (coe1 == coe2)

instance (Eq k, Ord k) => Ord (Kou k) where
  compare (Kou con1 coe1) (Kou con2 coe2) = compare coe2 coe1

canSum (Kou _ coe1) (Kou _ coe2) = coe1 == coe2

showCon 1 = ""
showCon (-1) = "-"
showCon x = show x
-- showCoeff　a _ = show a
showCoeff [] _ = ""
showCoeff (c:cs) (a:as)
  | c == 0    = showCoeff cs as
  | c == 1    = [a] ++ (showCoeff cs as)
  | otherwise = [a] ++ "^" ++ (show c) ++ (showCoeff cs as)

zipWithFill _ [] [] = []
zipWithFill filler (a:ls) [] = ((a,filler):zipWithFill filler ls [])
zipWithFill filler [] (a:ls) = ((filler,a):zipWithFill filler [] ls)
zipWithFill filler (a:as) (b:bs) = ((a,b):zipWithFill filler as bs)

(<<--) :: (Integral d, Num d) => (Kou d) -> [d] -> (Kou d)
k <<-- [] =  k
(Kou con coe) <<-- l = Kou (con * ncon) ncoe
  where
    (ncon, ncoe) = _apply coe l
    _apply [] _ = (1, [])
    _apply els [] = (1, els)
    _apply (e:el) (l:ls) = (l^e * next, (0:nl))
      where
        (next, nl) = _apply el ls
