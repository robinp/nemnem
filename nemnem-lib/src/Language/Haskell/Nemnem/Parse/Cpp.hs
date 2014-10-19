{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Nemnem.Parse.Cpp
  ( CppLines
  , isLineCppErased
  , isLineCppExpanded
  , unCpp
  ) where

import Control.Monad
import qualified Data.List as L
import qualified Data.Set as S
import Language.Preprocessor.Cpphs

data CppLines = CppLines
  { cppErased :: S.Set Int  -- ^ Note: could store as intervals, if worth it.
  , cppExpanded :: S.Set Int
  }
  deriving (Eq, Show)

isLineCppErased, isLineCppExpanded :: Int -> CppLines -> Bool
isLineCppErased n = S.member n . cppErased
isLineCppExpanded n = S.member n . cppExpanded

-- | Pity that cpphs runs in IO instead of something more abstract.
unCpp :: [(String, String)] -> String -> String -> IO (String, CppLines)
unCpp defines src_name source = {-# SCC unCpp #-} do
  let include_path = []
      bool_opts = defaultBoolOptions { hashline = False }
  transformed_src <- macroPass defines bool_opts
                       <=< cppIfdef src_name defines include_path bool_opts
                       $ source
  let cleaned_lines = rollup src_name transformed_src
      original_lines = lines source
      cpp_lines = diffLines original_lines cleaned_lines
  return (transformed_src, cpp_lines)

rollup :: String -> String -> [String]
rollup src_name src_with_line_pragmas = {-# SCC rollup #-}
  let src_lines = lines src_with_line_pragmas
  in go False src_lines []
  where
  go _ [] acc = reverse acc
  go should_emit (l:ls) acc =
    if "{-# LINE" `L.isPrefixOf` l then
      let emit = src_name `L.isInfixOf` l
      in go emit ls acc
    else
      let nacc = if should_emit then (l:acc) else acc
      in go should_emit ls nacc

diffLines :: [String] -> [String] -> CppLines
diffLines orig cppd = {-# SCC diffLines #-}
  go 1 orig cppd (CppLines S.empty S.empty)
  where
  go _ [] _ acc = acc
  go _ _ [] acc = acc
  go line (o:os) (p:ps) acc =
    if o == p then
      go (line+1) os ps acc
    else
      let nacc = if null p
                 then acc { cppErased = S.insert line $ cppErased acc }
                 else acc { cppExpanded = S.insert line $ cppExpanded acc }
      in go (line+1) os ps nacc
