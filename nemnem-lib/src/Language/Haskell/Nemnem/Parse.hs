{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Language.Haskell.Nemnem.Parse
  ( SourceInfo(..)
  , processModule
  , ProcessedModule(..)
  , ProcessModuleError(..)
  ) where

import Control.Exception (evaluate, try, SomeException)
import Control.Monad.IO.Class
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
import Language.Haskell.Nemnem.Parse.Cpp
import Hier (mkRange, transformRegions)

data SourceInfo = SourceInfo
  { siPackage :: String
  , siPackageVersion :: String
  , siPath :: FilePath
  }

data ProcessedModule = ProcessedModule
  { pmUntabbedSource :: String
  , pmUncppedSource :: String
  -- TODO make reverse-map (so also the  original file can be tagged eventually)
  , pmModifiedRegions :: Maybe [LineRegionModification]
  , pmModuleInfo :: ModuleInfo
  } deriving (Show)

data ProcessModuleError = ProcessModuleError
  { pmeUncppedSource :: String
  , pmeParseFailure :: String
  } deriving (Show)

processModule
  :: (MonadIO m)
  => [(String, String)]
  -> SourceInfo
  -> String  -- ^ Source content
  -> StateT (Map MName ModuleInfo) m (Either ProcessModuleError ProcessedModule)
processModule cpp_defines SourceInfo{..} raw_src = StateT $ \modules -> {-# SCC processModule #-} do
  let untabbed_src = unTab raw_src
  uncpp_result <- liftIO . try $ do
    res <- unCpp cpp_defines siPath untabbed_src
    evaluate (length . fst $ res) >> return res
  case uncpp_result of
    Left (ex :: SomeException) -> return (Left (ProcessModuleError "" (show ex)), modules)
    Right (uncpp_src, mb_region_mods) -> case {-# SCC hseParse #-} parse uncpp_src of
      fail@(ParseFailed _ _) -> do
        let err = ProcessModuleError
                    { pmeUncppedSource = uncpp_src
                    , pmeParseFailure = show fail }
        return (Left err, modules)
      ParseOk (ast, comments) -> do
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
            result = ProcessedModule
                       { pmUntabbedSource = untabbed_src
                       , pmUncppedSource = uncpp_src
                       , pmModifiedRegions = mb_region_mods
                       , pmModuleInfo = m_info }
        return (Right result, new_modules)
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
         -- , ignoreLinePragmas = False
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

