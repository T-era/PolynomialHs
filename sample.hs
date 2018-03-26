import Polynomial
import Term

_a = Kou 1 [1,0,0]
_b = Kou 1 [0,1,0]
_c = Kou 1 [0,0,1]
_f = Kou 1 ((take 5 $ repeat 0) ++ [1])
_g = Kou 1 ((take 6 $ repeat 0) ++ [1])
_h = Kou 1 ((take 7 $ repeat 0) ++ [1])

a = Shk [_a]
b = Shk [_b]
c = Shk [_c]
f = Shk [_f]
g = Shk [_g]
h = Shk [_h]

s = a^3 + f * a^2 + g * a + h

main = do
  -- 3次方程式に対する式変換で、2乗の項を消す。
  let tpo = 1/3 :: Double
      tto = 1/27 :: Double
      -- Doubleの項と式を定義する
      _db = Kou 1 [0,1] :: Kou Double
      _df = Kou 1 ((take 5 $ repeat 0) ++ [1]) :: Kou Double
      _dg = Kou 1 ((take 6 $ repeat 0) ++ [1]) :: Kou Double
      _dh = Kou 1 ((take 7 $ repeat 0) ++ [1]) :: Kou Double
      _dp = Kou 1 ((take 15 $ repeat 0) ++ [1])
      _dq = Kou 1 ((take 16 $ repeat 0) ++ [1])
      db = Shk [_db]
      df = Shk [_df]
      dg = Shk [_dg]
      dh = Shk [_dh]
      dp = Shk [_dp]
      dq = Shk [_dq]
      preF = (take 5 $ repeat 0)
      fa = db - (Shk [Kou tpo (preF ++ [1])])
      fg = dp + (Shk [Kou tpo (preF ++ [2])])
      fh = dq + (Shk [Kou tto (preF ++ [3])])
              + (Shk [(Kou tpo (preF ++ [1])) * _dp])

      ans = (s <<: [fa,0,0,0,0,df,fg,fh])

  putStrLn $ show s ++ " => " ++ show ans

  let
      -- 基本交代式
      p0 = a+b+c  -- 式変換で2乗の項を消してある場合には0になる
      p1 = a*b+b*c+c*a
      p2 = a*b*c

      a_b = a-b
      b_c = b-c
      c_a = c-a
      sssss = (a_b * b_c * c_a) ^ 2  -- 判別式(差積の自乗)

      ppppp = - 4 * p1^3 -- 判別式を基本交代式で構成してみる
              - 27 * p2^2
              - 4 * p2 * (p0^3)  -- 式変換で2乗の項を消してあると消える(p0 <- 0)
              + (p1^2) * (p0^2)  -- 式変換で2乗の項を消してあると消える(p0 <- 0)
              + 18 * p2 * p1 * p0  -- 式変換で2乗の項を消してあると消える(p0 <- 0)

      ans =  sssss - ppppp  -- 0(同じ式)
  putStrLn $ "(差積)^2 - " ++ show ppppp ++ " = " ++ show ans
