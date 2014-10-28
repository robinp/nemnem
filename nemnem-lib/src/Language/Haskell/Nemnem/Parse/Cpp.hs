{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Nemnem.Parse.Cpp
  ( RegionModification(..)
  , LineRegionModification(..)
  , unCpp
  ) where

import Control.Monad
import qualified Data.List as L
import qualified Data.Set as S
import Language.Preprocessor.Cpphs

-- | Pity that cpphs runs in IO instead of something more abstract.
unCpp :: [(String, String)] -> String -> String -> IO (String, Maybe [LineRegionModification])
unCpp defines src_name source = {-# SCC unCpp #-} do
  let include_path = []
      bool_opts = defaultBoolOptions { hashline = False }
  transformed_src <- macroPass defines bool_opts
                       <=< cppIfdef src_name defines include_path bool_opts
                       $ source
  return (transformed_src, rollup source transformed_src)

data RegionModification
  = Generated       -- ^ Mostly line pragmas, should be omitted from display.
  | Deleted String  -- ^ Might be displayed discretely.
  | IncludedBy String    -- ^ The #include macro line
  | ExpandedFrom String  -- ^ Original line before macro expansion.
  deriving (Eq, Show)

data LineRegionModification = ModLines
  { mlStart :: Int
  , mlEnd :: Int  -- ^ Exlusive
  , mlModification :: RegionModification
  } deriving (Eq, Show)

-- | Returns modified regions along with transformed line ranges (start inclusive,
-- end exclusive).
-- Nothing if unexpected line alignment happens.
rollup
  :: String  -- ^ Original file.
  -> String  -- ^ File after CPP transformation. (What about TH, eventually?)
  -> Maybe [LineRegionModification]  
rollup orig cppd =
  let orig_lines = lines orig
      cppd_lines = lines cppd
  in case cppd_lines of
       [] -> Nothing
       c:cs -> if isLinePragma c
              then go0 c orig_lines ([2..] `zip` cs) [ModLines 1 2 Generated]
              else Nothing
 where
  isLinePragma = ("{-# LINE" `L.isPrefixOf`)
  isInclude = ("#include" `L.isPrefixOf`)
  isPreprocessor = ("#" `L.isPrefixOf`)
  sameFileInLinePragma s t =
    let dropLineNum = drop 3 . words
    in dropLineNum s == dropLineNum t
  -- | orig_line_pragma is used to identify when an included section is over.
  go0 orig_line_pragma as0 ics0 = go True as0 ics0
   where
    -- | Bool arg == false iff currently in included context.
    go True [] [] acc = Just (reverse acc)

    go True (a:as) ((i,c):ics) acc =
      -- TODO verify if the include and the line pragma refer the same file.
      -- If not, then treat the line pragma as part of the original file.
      if isInclude a && isLinePragma c then
        let acc1 = (ModLines (i+1) (i+1) (IncludedBy a)):(ModLines i (i+1) Generated):acc
        in go False as ics acc1
      else if a /= c then
        let mod = if null c then Deleted a else ExpandedFrom a
            acc1 = (ModLines i (i+1) mod):acc
        in go True as ics acc1
      else 
        go True as ics acc  -- ^ Unmodified
          
    go False as ((i,c):ics) acc@((ModLines s _ incl@(IncludedBy _)):acc_tail) =
      if isLinePragma c && sameFileInLinePragma c orig_line_pragma then
        let acc1 = (ModLines i (i+1) Generated):acc
        in go True as ics acc1
      else
        let acc1 = (ModLines s (i+1) incl):acc_tail
        in go False as ics acc1
    
    go _ _ _ _ = Nothing
