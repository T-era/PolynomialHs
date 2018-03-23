import Polynomial
import Term

_a = Kou 1 [1,0,0]
_b = Kou 1 [0,1,0]
_c = Kou 1 [0,0,1]

a = Shk [_a]
b = Shk [_b]
c = Shk [_c]

main = do
  let
      -- 基本交代式
      p0 = a+b+c  -- フェラーリの方法だと消える
      p1 = a*b+b*c+c*a
      p2 = a*b*c

      a_b = a-b
      b_c = b-c
      c_a = c-a
      sssss = (a_b * b_c * c_a) ^ 2  -- 判別式(差積の自乗)

      ppppp = - 4 * p1^3 -- 判別式を基本交代式で構成してみる
              - 27 * p2^2
              - 4 * p2 * (p0^3)  -- フェラーリの方法だと消える(p0 <- 0)
              + (p1^2) * (p0^2)  -- フェラーリの方法だと消える(p0 <- 0)
              + 18 * p2 * p1 * p0  -- フェラーリの方法だと消える(p0 <- 0)

      ans =  sssss - ppppp  -- 消える(同じ式)
  putStrLn $ show ans
