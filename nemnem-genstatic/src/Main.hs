{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes,
             RecordWildCards, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State (StateT, execStateT)
import Data.Aeson ()
import qualified Data.ByteString as BS
import qualified Data.DList as DL
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Text.Blaze as B
import qualified Text.Blaze.Html.Renderer.Text as BR
import System.Environment
import Web.Scotty

import Language.Haskell.Nemnem.Parse.Module
import Language.Haskell.Nemnem.Parse
import Language.Haskell.Nemnem.Printer
import Language.Haskell.Nemnem.ProcessingPlan

-- | Quick hack for displaying a paired paren-rich string in a readable form.
prettish :: Int -> String -> String
prettish _ [] = []
prettish lvl (x:xs) = case x of
  o | o `elem` "([{" ->
    let lvl' = lvl + 1
    in '\n':o:(spaces lvl' ++ prettish lvl' xs)
  c | c `elem` ")]}" ->
    let lvl' = lvl - 1
    in c:prettish lvl' xs
  d | d == ',' ->
    d:'\n':(spaces lvl ++ prettish lvl xs)
  x -> x:prettish lvl xs
  where spaces x = replicate x ' '

moduleTransformLive = maybe "Anonymouse" TL.pack
moduleTransformStatic = (<> ".html") . moduleTransformLive

parseSourceInfoLine :: String -> SourceInfo
parseSourceInfoLine line = case words line of
  pkg_and_version:path:_ ->
    let (rev_ver, rev_pkg) = L.break (== '-') . reverse $ pkg_and_version
        pkg_version = reverse rev_ver
        pkg = case rev_pkg of
          '-':xs -> reverse xs
          _ -> error $ "Extracting package version: " ++ line
    in SourceInfo pkg pkg_version path
  _ -> error $ "parseSourceInfoLine: " ++ line

data ProcessModuleConfig = ProcessModuleConfig
  { pmcOutDir :: String
  , pmcCppDefines :: [(String, String)]
  }

main = do
  args <- getArgs
  source_infos <- dagOrdered siPath =<< case args of
    [] -> return []
    (path_file:_) -> map parseSourceInfoLine
                       . filter (not . ("#" `L.isPrefixOf`))
                       . lines
                       <$> readFile path_file
  let pkg_defines = definesFromPackages source_infos
      hacky_defines =
        -- TODO LANGUAGE_xy ?
        -- TODO defines from args/cfg
        [ ("UseGHC", "1")
        , ("__GLASGOW_HASKELL__", "783")
        , ("INTEGER_GMP", "1")
        , ("SIZEOF_HSWORD", "4")
        ]
      config = ProcessModuleConfig "deploy/" $ concat [hacky_defines, pkg_defines]
  print pkg_defines
  module_infos <- mapM_ (processModules config) source_infos `execStateT` M.empty
  return ()
  {-
  scotty 8080 $ do
    get "/static/:resource" $ do
      -- TODO this is possibly unsecure
      -- TODO mime types
      resource_path <- param "resource"
      file $ "deploy/static/" ++ resource_path  -- TODO
    get "/module/list" $ do
      json . M.keys $ module_infos
    get "/module/source/get/:name" $ do
      module_name <- param "name"
      -- TODO more RPC-ish (EitherT SomeError IO Res) with auto error handling
      mb_result <- lift . runMaybeT $ do
        module_info <- hoistMaybe $ M.lookup module_name module_infos
        src_path <- hoistMaybe $ miOriginalPath module_info
        src <- lift $ readFile src_path 
        return $ renderTaggedHtml moduleTransformLive src module_info
      case mb_result of
        Nothing -> raise "module or source not found"
        Just result -> html result
    get "/module/localrefs/get/:name" $ do
      module_name <- param "name"
      case M.lookup module_name module_infos of
        Nothing -> raise "module not found"
        Just module_info -> do
          json $ miRefs module_info
    -}
  where
  hoistMaybe :: Maybe a -> MaybeT IO a
  hoistMaybe = MaybeT . return
  -- TODO move to lib/Parse.hs, tear to pieces..
  processModules :: ProcessModuleConfig
                 -> SourceInfo
                 -> StateT (Map MName ModuleInfo) IO ()
  processModules ProcessModuleConfig{..} source_info = {-# SCC processModules #-} do
    raw_src <- {-# SCC readsource #-} lift
            -- TODO custom lenientDecode which also debug-logs error
            . fmap (T.unpack . T.decodeUtf8With TE.lenientDecode)
            . BS.readFile
            $ siPath source_info
    lift . putStrLn . siPath $ source_info
    err_or_res <- processModule pmcCppDefines source_info raw_src
    lift . putStrLn $ "Finished"
    case err_or_res of
      Left (ProcessModuleError{..}) -> lift $ do
        print pmeParseFailure
        -- TODO guard with flag
        unless (null pmeUncppedSource) $
          writeCppd pmcOutDir pmeUncppedSource (siPath source_info ++ ".cpp")
      Right (ProcessedModule{..}) -> lift $ do
        print . miName $ pmModuleInfo
        -- print pmModuleInfo
        -- print pmModifiedRegions
        let source_to_link = pmUncppedSource
        writeCppd pmcOutDir source_to_link (siPath source_info ++ ".cpp")
        -- TODO write a combination of untabbed/cppd source for full info
        --      content
        writeLinked pmcOutDir source_to_link pmModuleInfo pmNonCrossrefInfo
    lift . putStrLn $ "Done"
  --
  writeLinked :: String -> String -> ModuleInfo -> NonCrossRefInfo -> IO ()
  writeLinked outdir src mi nxi = {-# SCC writeLinked #-} do
    let fn = outdir ++ fromMaybe "Anonymous" (miName mi) ++ ".html"
    res <- try 
           . TL.writeFile fn
           $ renderTaggedHtml moduleTransformStatic src mi nxi
    case res of
      Left (ex :: SomeException) ->
        putStrLn $ "Error hyperlinking " ++ fn ++ ", error: " ++ show ex
      Right () -> return ()
  -- | TODO remove when not needed
  writeCppd :: String -> String -> FilePath -> IO ()
  writeCppd outdir src to_path = TL.writeFile to_path . TL.pack $ src
  --
  moduleRanges :: String -> ModuleInfo -> NonCrossRefInfo -> [Range Tag]
  moduleRanges src ModuleInfo{..} NonCrossRefInfo{..} = {-# SCC moduleRanges #-}
    let lineLens = map ((+1) . length) (lines src)
        bases = basesOf miRefs miExports
        ranges = let ofsFun = offsetAt (mkOffsetTable lineLens)
                     refs = map (refToRange ofsFun) miRefs
                     entities =
                       mapMaybe (tagEntitiesOfCurrentModule ofsFun miName)
                                bases
                     warns = map (warnsToRange ofsFun) miWarns
                     hlits = map (highlightsToRange ofsFun) miHighlights
                 in DL.toList . DL.concat . map DL.fromList $
                      [refs, entities, warns, hlits]
    in ranges
  --
  renderTaggedHtml :: (Maybe MName -> TL.Text) -> String -> ModuleInfo -> NonCrossRefInfo -> TL.Text
  renderTaggedHtml module_transform src mi nxi = {-# SCC renderTaggedHtml #-}
    -- assumes newline is \n (single char)
    let ranges = moduleRanges src mi nxi
        tagTransformer = tagToBlaze module_transform (miName mi)
        tagged = tagRanges tagTransformer ranges (T.pack src)
    in BR.renderHtml . withHeader $ tagged
  break = putStrLn "---"
  putStrBreak x = break >> putStrLn x

-- TODO these methods rather belong to nemnem-lib/.../Printer.hs
(Range _ a b) `contains` (Range _ c d) = a <= c && d <= b
modifyR f (Range t a b) = Range (f t) a b
getR (Range t _ _) = t
startR (Range _ s _) = s
endR (Range _ _ e) = e

-- |
-- Note: could check if using Text.Lazy could lower the memory usage
-- TODO this can blow up if the ranges are messed up, do some checking
tagRanges
  :: (a -> B.Markup -> B.Markup)
  -> [Range a]
  -> T.Text
  -> B.Markup
tagRanges blazer rs txt0 = {-# SCC tagRanges #-}
  let fs = map (modifyR blazer) rs
      ordered = L.sortBy (comparing (\(Range _ s e) -> (s,-e))) fs
      tlen = T.length txt0
      full = Range undefined 0 tlen
  in go ((0, txt0), [(mempty, full)]) ordered
  where
  -- TODO move to other module, add meaningful type aliases
  -- | A gap is an untagged piece of text.
  go :: ((Int, T.Text),  -- ^ Remaining text and the position of its first char.
         [(B.Markup,  -- ^ Stack, top is markup accumulator and its range,
           Range (B.Markup -> B.Markup))])  -- ^ holding transform to be applied
                              -- The accumulator spans from the range start, and
                              -- eventually reaches the end. Then the top is popped.
     -> [Range (B.Markup -> B.Markup)]  -- ^ Ranges according to pre-order ordering.
     -> B.Markup  -- ^ Marked text.

  go ((idx, txt), stk) [] = case stk of
    (acc, r):[] -> 
      let (gap, _) = gapAndRest (endR r - idx) txt
      in acc <> gap
    (acc1, r1):(acc2, r2):stks ->
      pop idx txt acc1 r1 acc2 r2 stks []
  go ((idx, txt), stk) rr@(r:rs) = case stk of
    [] -> error "Stack can't be empty (`full` should remain)"
    (acc, r1):stks | r1 `contains` r ->
      -- record start-gap, push r
      let gap_size = startR r - idx
          (gap, txt1) = gapAndRest gap_size txt
      in go ((idx + gap_size, txt1),
            (mempty, r):(acc <> gap, r1):stks) rs
    (acc1, r1):(acc2, r2):stks ->
      pop idx txt acc1 r1 acc2 r2 stks rr
    _ -> error "Unexpected range setup?"
  pop idx txt acc1 r1 acc2 r2 stks rr =
    -- Add end-gap to r1, pop off r1, and execute it and record the result
    -- in acc2. Retry same range input (with the updated state).
    let gap_size = endR r1 - idx
        (gap, txt1) = gapAndRest gap_size txt
        result1 = getR r1 $ acc1 <> gap
    in go ((idx + gap_size, txt1),
          (acc2 <> result1, r2):stks) rr
  gapAndRest :: Int -> T.Text -> (B.Markup, T.Text)
  gapAndRest gap_size txt =
    let (gap, txt1) = T.splitAt gap_size txt
        blaze_gap = if T.null gap then mempty else toBlaze gap
    in (blaze_gap, txt1)
