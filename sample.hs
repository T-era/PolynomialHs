import Polynomial
import Term

import Data.Char

-- 一乗の指定した文字項を作ります
ms n c = monomial n l
  where
    l = (take at $ repeat 0) ++ [1]
    at = (ord c) - (ord 'a')

a = ms 1 'a'
b = ms 1 'b'
c = ms 1 'c'
f = ms 1 'f'
g = ms 1 'g'
h = ms 1 'h'

s = a^3 + f * a^2 + g * a + h

main = do
  -- 3次方程式に対する式変換で、2乗の項を消す。
  let -- Doubleの項と式を定義する
      df = ms 1 'f'
      fa = ms 1 'b' - (ms (1/3) 'f')
      preF = (take 5 $ repeat 0)
      fg = (ms 1 'p') + 3 * (ms (1/3) 'f') ^ 2
      fh = (ms 1 'q') + (ms (1/3) 'f') ^ 3
              + (ms (1/3) 'f') * (ms 1 'p')

      ans = (s <<: [fa,0,0,0,0,df,fg,fh])

  putStrLn $ show s ++ " => " ++ show ans

  let
      -- 基本対称式
      p0 = a+b+c  -- 式変換で2乗の項を消してある場合には0になる
      p1 = a*b+b*c+c*a
      p2 = a*b*c

      a_b = a-b
      b_c = b-c
      c_a = c-a
      sssss = (a_b * b_c * c_a) ^ 2  -- 判別式(差積の自乗)

      ppppp = - 4 * p1^3 -- 判別式を基本対称式で構成してみる
              - 27 * p2^2
              - 4 * p2 * (p0^3)  -- 式変換で2乗の項を消してあると消える(p0 <- 0)
              + (p1^2) * (p0^2)  -- 式変換で2乗の項を消してあると消える(p0 <- 0)
              + 18 * p2 * p1 * p0  -- 式変換で2乗の項を消してあると消える(p0 <- 0)

      ans =  sssss - ppppp  -- 0(同じ式)
  putStrLn $ "(差積)^2 - " ++ show ppppp ++ " = " ++ show ans
