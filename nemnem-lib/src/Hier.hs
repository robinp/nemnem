{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Hier 
  ( LengthSplitAt
  , TaggedRange()
  , mkRange
  , showTaggedRange
  , mkMarker
  , tagRegions
  , untag
  , transformRegions
  ) where

import Data.Int (Int64)
import Data.List (sortBy, splitAt)
import Data.Ord (comparing)
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

data Tagged a e = Tagged
  { untagged :: e
  , tagged :: Maybe (TagTail e a)
  } deriving (Show)

data TagTail e a = TagTail
  { tag :: a
  , region :: Tagged a e
  , rest :: Tagged a e 
  } deriving (Show)

data TaggedRange e a = TaggedRange
  { rangeTag :: a
  , rangeStart :: Length e
  , rangeEnd :: Length e
  }

showTaggedRange :: (Show a, Show (Length e)) => TaggedRange e a -> String
showTaggedRange tr = show (rangeTag tr) ++ " (" ++ show (rangeStart tr) ++
                       ", " ++ show (rangeEnd tr)

class (Num (Length a), Ord (Length a)) => LengthSplitAt a where
  type Length a
  lsLength :: a -> Length a
  lsSplitAt :: Length a -> a -> (a, a)

-- TODO rather call Splittable?
instance LengthSplitAt [a] where
  -- TODO rather call Index?
  type Length [a] = Int
  lsLength = length
  lsSplitAt = splitAt

instance LengthSplitAt BL.ByteString where
  type Length BL.ByteString = Int64
  lsLength = BL.length
  lsSplitAt = BL.splitAt

instance LengthSplitAt TL.Text where
  type Length TL.Text = Int64
  lsLength = TL.length
  lsSplitAt = TL.splitAt

instance LengthSplitAt T.Text where
  type Length T.Text = Int
  lsLength = T.length
  lsSplitAt = T.splitAt

instance Functor (Tagged a) where
  fmap f (Tagged e tl) = Tagged (f e) (fmap (maptail f) tl)
    where maptail f (TagTail a r s) = TagTail a (fmap f r) (fmap f s)

-- TODO
--mapContent :: ([e] -> [e]) -> ...
--mapTags :: (a -> b) -> ...

mkRange :: a -> Length e -> Length e -> TaggedRange e a
mkRange = TaggedRange

mkMarker :: a -> Length e -> TaggedRange e a
mkMarker a l = TaggedRange a l l

-- TODO why is it not transformRanges ?
transformRegions :: (Monoid e, LengthSplitAt e) =>
                    (a -> e -> e) -> [TaggedRange e a] -> e -> e
transformRegions t rs s = untag t $ tagRegions rs s

subRange :: (LengthSplitAt e) => Length e -> TaggedRange e a -> TaggedRange e a
subRange s (TaggedRange a x y) = TaggedRange a (x-s) (y-s)

tlength :: (LengthSplitAt e) => Tagged a e -> Length e
tlength (Tagged ut t) =
  let hlen = lsLength ut
      tlen = case t of
        Nothing -> 0
        Just (TagTail _ r s) -> tlength r + tlength s
  in {-# SCC "tlength" #-} hlen + tlen

-- TODO make untagging go through third type (BS.Builder for example)
untag :: (Monoid e) => (a -> e -> e) -> Tagged a e -> e
untag f (Tagged ut t) = {-# SCC "untag" #-} case t of
  Nothing -> ut
  Just (TagTail a reg rst) ->
    ut `mappend` f a (untag f reg) `mappend` untag f rst

-- | Assumes non-crossing regions, good boundaries. Subregions are allowed.
tagRegions :: (LengthSplitAt e) =>[TaggedRange e a] -> e -> Tagged a e
tagRegions ranges elems =
  let ordered = {-# SCC "sort" #-} sortBy (comparing (\x -> rangeStart x - rangeEnd x)) ranges
  in {-# SCC "tagRegions" #-} foldl tag0 (Tagged elems Nothing) ordered

tag0 :: (LengthSplitAt e) => Tagged a e -> TaggedRange e a -> Tagged a e
tag0 orig@(Tagged ut t) r@(TaggedRange a start end) = 
  let rlen = end - start
      prefixLen = {-# SCC "lsLength" #-} lsLength ut
  in {-# SCC "tag0" #-} if end <= prefixLen then
       let (as, bcs) = lsSplitAt start ut
           (bs, cs) = lsSplitAt rlen bcs
           tagtail = Just$ TagTail a (Tagged bs Nothing) rst
           rst = Tagged cs t
       in {-# SCC "endBefore" #-} Tagged as tagtail
     else if start < prefixLen then orig  -- bad input, overlapping
     else case t of
       Nothing -> orig  -- bad input
       Just tl@(TagTail _ tReg tRest) ->
         let newRange = subRange prefixLen r
             regLength = {-# SCC "regLength" #-} tlength tReg
             newTail = {-# SCC "newTail" #-} if rangeEnd newRange <= regLength
               then tl { region = tag0 tReg newRange }
               else let newestRange = subRange regLength newRange
                    in tl { rest = tag0 tRest newestRange }
         in {-# SCC "endAfter" #-} orig { tagged = Just newTail }
