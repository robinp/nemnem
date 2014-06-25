module Language.Haskell.Nemnem.Util where

cond c a b = if c then a else b

modifyIf c f = cond c f id

invertIf c = modifyIf c not

mapFst f (a,b) = (f a,b)
mapSnd f (a,b) = (a,f b)
