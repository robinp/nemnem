module Language.Haskell.Nemnem.Parse where

import Control.Monad.Trans.State
import qualified Data.Array as A
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Language.Haskell.Exts.Annotated
import Text.Regex.TDFA

import Language.Haskell.Nemnem.Internal.Util
import Language.Haskell.Nemnem.Parse.Module
import Hier (mkRange, transformRegions)

processModule
  :: (Monad m)
  => (FilePath, String)    -- ^ Path and content of module
  -> StateT (Map MName ModuleInfo) m (Either String ModuleInfo)
processModule (path, raw_src) = StateT $ \modules ->
  let src = unCpp raw_src
  in case parse src of
       fail@(ParseFailed _ _) -> return (Left (show fail), modules)
       ParseOk (ast, comments) ->
         -- TODO preprocess comment locations and pass to collectModule, so it can
         --      link comments to definitions.
         let comment_hls = map makeCommentHighlight comments
             m_info0 = (collectModule modules ast)
                         { miOriginalPath = Just path }
             m_info = m_info0
                        { miHighlights = miHighlights m_info0 ++ comment_hls}
             new_modules = M.insert
                             (fromMaybe "Anonymous" . miName $ m_info)
                             m_info
                             modules
         in return (Right m_info, new_modules)
  where
  -- switch off fixities - this results in possibly incorrect AST, but
  -- on the source highlighting / indexing level we don't care about
  -- larger expressions anyway (might be needed for gradient highlight).
  parse = parseFileContentsWithComments $ defaultParseMode 
            { fixities = Nothing
            , parseFilename = path
            -- TODO user-suppliable list of extra LANG extensions
            }

-- | Offset-preserving removal of CPP-directives and possible macro calls.
unCpp :: String -> String
unCpp = unlines . reverse . trd . foldl replace_cpp (False, False, []) . lines
  where
  replace_cpp (was_where, inside_def, res) l0 =
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
       then let replaced = replicate (length l) ' '  -- length-preserving transform
            in (new_was_where, ends_in_slash, replaced:res)
       else (new_was_where, False, l:res)
    where
    -- | Heuristic to get rid of CHECK_BOUNDS(x,y,z) like invocations.
    -- This is only correct for no-op assert macros, which is not always the case.
    -- Otherwise parsing will result in error. Oh my.
    removeMacroCall :: String -> String
    removeMacroCall l = let matches = matchAll pattern l :: [MatchArray] -- Skip imports since an import `SomeModule (XY(a,b,c..))` would match
      in if "import" `L.isPrefixOf` l || null matches then l
         else let offset_and_lengths = concat . map (A.elems) $ matches
                  regions = map (\(off,len) -> mkRange () off (off+len))
                              offset_and_lengths
              in transformRegions (\_ e -> replicate (length e) ' ') regions l
      where
      pattern = makeRegex ("[A-Z][A-Z0-9_]+\\([^)]*\\)" :: String)  :: Regex
    
