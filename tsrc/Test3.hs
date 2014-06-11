module Test3 where

import Test4 hiding (One)
import qualified Test4 as T4

data MyData = Case1 | Case2 String MyData | MyRec MyRecord

data MyRecord = Record
  { apples :: Int
  , pies, oranges :: OtherRec
  }

data OtherRec = Other MyRecord

hiss :: String -> String
hiss s = "Hisss " ++ s

fst' a b = a
xyz r = apples r + foo 1 + T4.bar 2

f :: MyData -> String
f Case1 = "Found one"
f (Case2 x _) = "Found " ++ x

rec1 x = rec2 (x+1)

rec2 x = rec1 (x-1)

localRec x =
  let y = x+z
      z = y
  in undefined

g,h :: MyData -> Int -> Int
g x y = case x of
  Case1 -> 1
  Case2 s _ -> length s + y

h = undefined
i = 12

i1 :: DAbs
i1 = undefined

i2 :: T4.DAll
i2 = if True then Me else AndMe

i3 :: DRecAll -> Int
i3 r = T4.drA r + drB r

i4 :: a -> T4.DSome
i4 x = One undefined

{-
-- negatives
xx r = drNotExported r
xy = NotThree
-}
