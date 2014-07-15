{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Nemnem.Parse
  ( SourceInfo(..)
  , processModule
  ) where

import Control.Monad.Trans.State
import qualified Data.Array as A
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Language.Haskell.Exts.Annotated
import Text.Regex.TDFA

import Language.Haskell.Nemnem.Internal.Source
import Language.Haskell.Nemnem.Internal.Util
import Language.Haskell.Nemnem.Parse.Module
import Hier (mkRange, transformRegions)

data SourceInfo = SourceInfo
  { siPackage :: String
  , siPackageVersion :: String
  , siPath :: FilePath
  }

-- | Returns also the parsed src on success.
processModule
  :: (Monad m)
  => [String]
  -> SourceInfo
  -> String  -- ^ Source content
  -> StateT (Map MName ModuleInfo) m (Either String (String, ModuleInfo))
processModule id_macro_prefixes SourceInfo{..} raw_src = StateT $ \modules ->
  {-# SCC processModule #-} 
  let parsed_src = unTab raw_src
      src = {-# SCC sourceTransform #-} unCpp id_macro_prefixes parsed_src
  in case {-# SCC hseParse #-} parse src of
       fail@(ParseFailed _ _) -> return (Left (show fail), modules)
       ParseOk (ast, comments) ->
         -- TODO preprocess comment locations and pass to collectModule, so it can
         --      link comments to definitions.
         let comment_hls = {-# SCC comments #-}
                           map makeCommentHighlight comments
             m_info0 = (collectModule modules ast)
                         { miOriginalPath = Just siPath }
             m_info = {-# SCC concat #-} m_info0
                        { miHighlights = miHighlights m_info0 ++ comment_hls}
             new_modules = M.insert
                             (fromMaybe "Anonymous" . miName $ m_info)
                             m_info
                             modules
         in return (Right (parsed_src, m_info), new_modules)
  where
  parse =
    let forced_exts =
          [ 
            -- https://github.com/haskell-suite/haskell-src-exts/issues/29,
            -- and these can't hurt anyway
            EnableExtension MultiParamTypeClasses
          , EnableExtension FlexibleContexts
          , EnableExtension FlexibleInstances
          ]
        exts =
          let ge = moGlasgowExts . moduleOptions . T.lines . T.pack $ raw_src
          in if ge then glasgowExts else []
    in parseFileContentsWithComments $ defaultParseMode 
         -- switch off fixities - this results in possibly incorrect AST, but
         -- on the source highlighting / indexing level we don't care about
         -- larger expressions anyway (might be needed for gradient highlight).
         { fixities = Nothing
         , parseFilename = siPath
         -- TODO user-suppliable list of extra LANG extensions
         , extensions = forced_exts ++ exts
         }

-- | HSE would interpret a tab as 8 position indent, and would return funny
-- source columns, which interfers with text-based tagging. So take initiative
-- and convert tabs to 8 spaces.
unTab :: String -> String
-- TODO could receive and leave as (lazy) Text instead roundtripping
unTab = TL.unpack . TL.replace tab spaces . TL.pack
  where
  tab = TL.pack "\t"
  spaces = TL.pack (replicate 8 ' ')

-- | Offset-preserving removal of CPP-directives and possible macro calls.
-- TODO could support basic ifdefs like version checks.. but even nicer would be
--      a cpphs-like package which returns a range mapping between the original
--      and the transformed source, so the transformed could be analyzed, but
--      results would be marked up on the original.
unCpp :: [String] -> String -> String
unCpp id_macro_prefixes = {-# SCC unCpp #-}
  unlines 
  . reverse . trd
  . foldl replaceCpp (False, False, [])
  . lines
  where
  replaceCpp (was_where, inside_def, res) l0 = {-# SCC replaceCpp #-}
    -- TODO emit preprocessor ranges for syntax highlight
    --
    -- If we didn't encounter the `where` keyword of the module, don't try to
    -- strip macro-like calls since it can match export patterns.
    let l = if was_where then removeMacroCall l0 else l0
        ends_in_slash = "\\" `L.isSuffixOf` l
        start_def = "#" `L.isPrefixOf` l
        -- TODO this is pretty fragile, could look for `module X (.....) where`
        new_was_where = was_where || "where" `L.isInfixOf` l
    in if start_def || inside_def
       then let replaced =
                  -- Length-preserving transform.
                  replicate (length l) ' '
            in (new_was_where, ends_in_slash, replaced:res)
       else (new_was_where, False, l:res)
    where
    -- | Heuristic to get rid of CHECK_BOUNDS(x,y,z) like invocations.
    removeMacroCall :: String -> String
    removeMacroCall l = {-# SCC removeMacroCall #-}
      let matches = matchAll pattern l :: [MatchArray] -- Skip imports since an import `SomeModule (XY(a,b,c..))` would match
      in if "import" `L.isPrefixOf` l || null matches then l
         else let offset_and_lengths =
                    map modifyOffsetForNestedParens
                    . filter canRemoveMacroCall
                    . concat . map (A.elems)
                    $ matches
                  regions = map (\(off,len) -> mkRange () off (off+len))
                              offset_and_lengths
              in transformRegions (\_ e -> replicate (length e) ' ') regions l
      where
      pattern = makeRegex ("[A-Z][A-Z0-9_]+\\([^)]*\\)" :: String)  :: Regex
      canRemoveMacroCall (offset, _) =
        let frag = takeWhile (/= '(') . drop offset $ l
        in (`L.isPrefixOf` frag) `any` id_macro_prefixes
      modifyOffsetForNestedParens (offset, _) =
        let (name, rest) = L.break (== '(') . drop offset $ l
        in (offset, length name + parlen 0 0 rest)
      parlen i cnt xs = case (cnt, xs) of
        (_, '(':rs) -> parlen (i+1) (cnt+1) rs
        (1, ')':_)  -> i+1
        (x, ')':rs) -> parlen (i+1) (x-1) rs
        (x, _:rs)   -> parlen (i+1) x rs
        _ -> i
