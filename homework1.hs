{-# OPTION -Wall #-}
module Hw where

abs' :: Int -> Int
abs' x = if x<0 then -x else x
abs'' :: Int -> Int
abs'' x
  |x<0 = -x
  |otherwise = x

clip :: Int -> Int  -> Int -> Int
clip d u x
  |x<dm = dm
  |x > um = um
  |otherwise = x
  where
    dm = min d u
    um = max d u

clip' :: Int -> Int -> Int -> Int
clip' d u = min (max d u)  . max (min d u)

xyzzy :: Double -> Int
xyzzy x = negate (ceiling (abs (cos x)))

xyzzy' :: Double -> Int
xyzzy' x = negate $ ceiling $ abs $ cos x

xyzzy'' :: Double -> Int
xyzzy'' = negate . ceiling . abs . cos

sinTaylor :: Double -> Int -> Int -> Int -> Double -> Double -> Double
sinTaylor x n count factorial power acc
  |n == 0 = acc-power/(fromIntegral $ count*(count+1))
  |otherwise = sinTaylor x (n-1) nc nf np nacc
  where
    nc = count+1
    nf = factorial*nc*(nc+1)
    np =  -power*(x^2)
    nacc = acc+(power/(fromIntegral factorial))

sinTaylor' :: Double -> Int -> Double
sinTaylor' x n = sinTaylor x n 1 1 x 0
