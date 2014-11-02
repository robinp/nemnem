{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Language.Haskell.Nemnem.Parse
  ( SourceInfo(..)
  , definesFromPackages
  , processModule
  , ProcessedModule(..)
  , ProcessModuleError(..)
  ) where

import Control.Exception (evaluate, try, SomeException, ErrorCall(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.List.Split as L
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
  , pmNonCrossrefInfo :: NonCrossRefInfo
  } deriving (Show)

data ProcessModuleError = ProcessModuleError
  { pmeUncppedSource :: String
  , pmeParseFailure :: String
  } deriving (Show)

-- | From Cabal/Distribution/Simple/Build/Macros.hs .
-- Assumes, that each package has a single version.
definesFromPackages :: [SourceInfo] -> [(String, String)]
definesFromPackages sis =
  let uniques = L.nub . map pkgInfo $ sis
  in uniques >>= packageDefines
  where
  pkgInfo SourceInfo{..} = (siPackage, siPackageVersion)
  packageDefines (pkg0, ver) =
    let pkg = L.intercalate "_" . L.splitOn "-" $ pkg0
        ver_define = genVersion pkg ver
    in case extractVersion ver of
        Just v  -> [ver_define, genMinVersion pkg v]
        Nothing -> [ver_define]
  genVersion pkg ver = ("VERSION_" ++ pkg, ver)
  genMinVersion pkg (a,b,c) = 
    let key = "MIN_VERSION_" ++ pkg ++ "(major1,major2,minor)"
        value = concat
          [ "(\\\n"
          , "  (major1) <  ", a, " || \\\n"
          , "  (major1) == ", a, " && (major2) <  ", b, " || \\\n"
          , "  (major1) == ", a, " && (major2) == ", b, " && (minor) <= ", c, ")"
          ]
    in (key, value)
  extractVersion :: String -> Maybe (String, String, String)
  extractVersion ver = case L.splitOn "." ver of
    a:b:c:_ -> Just (a,b,c)
    [a,b]   -> Just (a,b,"0")
    _       -> Nothing

processModule
  :: (MonadIO m)
  => [(String, String)]
  -> SourceInfo
  -> String  -- ^ Source content
  -> StateT (Map MName ModuleInfo) m (Either ProcessModuleError ProcessedModule)
processModule cpp_defines SourceInfo{..} raw_src = StateT $ \modules -> {-# SCC processModule #-} do
  let untabbed_src = unTab raw_src
  -- TODO move error handling inside unCpp
  uncpp_result <- liftIO . try $ do
    res <- unCpp cpp_defines siPath untabbed_src
    evaluate (length . fst $ res) >> return res
  case uncpp_result of
    Left (ex :: SomeException) -> return (Left (ProcessModuleError "" (show ex)), modules)
    Right (uncpp_src, mb_region_mods) -> do
      parse_result <- liftIO . try . evaluate $ {-# SCC hseParse #-} parse uncpp_src
      case parse_result of
        Left (ErrorCall e) -> return (Left (ProcessModuleError uncpp_src e), modules)
        Right res -> case res of
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
                (m_info0, nonxref_info0) = (collectModule modules ast)
                m_info = m_info0 { miOriginalPath = Just siPath }
                nonxref_info = {-# SCC concat #-} nonxref_info0
                  { miHighlights = miHighlights nonxref_info0 ++ comment_hls}
                new_modules = M.insert
                                (fromMaybe "Anonymous" . miName $ m_info)
                                m_info
                                modules
                result = ProcessedModule
                           { pmUntabbedSource = untabbed_src
                           , pmUncppedSource = uncpp_src
                           , pmModifiedRegions = mb_region_mods
                           , pmModuleInfo = m_info
                           , pmNonCrossrefInfo = nonxref_info
                           }
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
            -- https://github.com/haskell-suite/haskell-src-exts/issues/133
          , EnableExtension NondecreasingIndentation  -- should be definitely per-module
            -- let's just throw these in for now
            -- TODO from config, maybe per package on module
          , EnableExtension TypeOperators
          , EnableExtension FunctionalDependencies
          , EnableExtension TypeFamilies
          , EnableExtension ScopedTypeVariables
          , EnableExtension BangPatterns
          , EnableExtension RecordWildCards
          , EnableExtension EmptyDataDecls
          , EnableExtension DeriveDataTypeable
          , EnableExtension GADTs
          , EnableExtension StandaloneDeriving
          , EnableExtension ExplicitForAll
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

-- | From Haskell Report 9.3:
-- "Tab stops are 8 characters apart."
-- "A tab character causes the insertion of enough spaces to align the current
--  position with the next tab stop."
unTab :: String -> String
-- TODO could receive and leave as (lazy) Text instead roundtripping
unTab src =
  let t = TL.pack src
  in if not (hasTab t) then src
     else TL.unpack . convertTabs $ t
  where
  isTab = (== '\t')
  hasTab = TL.any isTab
  convertTabs = TL.unlines . map convertLine . TL.lines
  convertLine t =
    if TL.null t then t
    else TL.concat . reverse . snd . foldl go (0, []) . TL.split isTab $ t
    where
    go :: (Int, [TL.Text]) -> TL.Text -> (Int, [TL.Text])
    go (col0, acc) frag =
      let col = col0 + (fromIntegral . TL.length) frag
          (padsize, pad) = if col /= 0 || TL.null frag
            then (skip col, TL.pack $ replicate (skip col) ' ')
            else (0, TL.empty)
      in (col + padsize, pad:frag:acc)
      where
      skip col =
        let y = col + 8
            m = y `mod` 8
        in y - m - col
