module Test4
  ( DAbs
  , DAll(..)
  , DRecAll(..)
  , DRec(DRec)  -- no fields
  , DSome(One, Two)
  , foo
  , bar
  ) where

foo x = 1 :: Int
bar y = 2 :: Int

lam :: Int -> Int
lam x = 2 * x

data DAbs = NotExported
data DAll = Me | AndMe
data DRec = DRec { drNotExported :: Int }
data DRecAll = DRecAll { drA :: Int, drB :: Int }
data DSome = One String | Two Int Int | NotThree
