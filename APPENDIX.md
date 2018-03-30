## 式変換と代入

### 浮動小数の文字式
``` haskell
monomial :: Double -> [Integer] -> Shk Double
```
を使うと、浮動小数点数を係数にした式を手軽に作成できます。
``` haskell
> fromDouble 3.5 [1]
+3.5a
```
計算(和/差/積)は、係数が浮動小数点数でも可能です。これは計算の型制約が Num (と、まあ細かいいくつか)だけだからです。
`(<<-)`による代入も同様に、 Num であれば可能です。
一方で`(<<:)`の代入は(代入の引数値との演算ができる必要があるため)、Integral である必要があります。

### 係数の変換

Shk は Functor なので、fmapで係数を変換することができます。
``` haskell
> fmap truncate (monomial 12.3 [1])
+12a
> fmap truncate (monomial 12.3 [1]) <<: [3]
36
```
