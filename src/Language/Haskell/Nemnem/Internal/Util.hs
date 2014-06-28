module Language.Haskell.Nemnem.Internal.Util where

cond :: Bool -> a -> a -> a
cond c a b = if c then a else b

-- | Returns identity op if argument is False. 
modifyIf :: Bool -> (a -> a) -> (a -> a)
modifyIf c f = cond c f id

invertIf :: Bool -> (Bool -> Bool)
invertIf c = modifyIf c not

mapFst f (a,b) = (f a,b)
mapSnd f (a,b) = (a,f b)

fst3 (a,_,_) = a
snd3 (_,a,_) = a
trd (_,_,a) = a
