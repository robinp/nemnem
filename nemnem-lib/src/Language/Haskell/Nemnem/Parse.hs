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
  => [(String, String)]
  -> SourceInfo
  -> String  -- ^ Source content
  -> StateT (Map MName ModuleInfo) m (Either String (String, ModuleInfo))
processModule cpp_defines SourceInfo{..} raw_src = StateT $ \modules ->
  {-# SCC processModule #-} 
  let parsed_src = unTab raw_src
      -- TODO: * exec new unCpp in IO
      --       * in Parse/Module.hs, discard references that originate from
      --         included files (hint: different filename in SrcInfo)
      --       * and someone should skip references to/from cppExpanded lines
      src = {-# SCC sourceTransform #-} parsed_src -- unCpp cpp_defines parsed_src
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
            -- DOH, let's just throw these in for now
          , EnableExtension TypeOperators
          , EnableExtension FunctionalDependencies
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

