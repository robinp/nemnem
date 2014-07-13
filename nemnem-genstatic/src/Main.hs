{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes,
             OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State (StateT, execStateT)
import Data.Aeson ()
import qualified Data.DList as DL
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Text.Blaze as B
import qualified Text.Blaze.Html.Renderer.Text as BR
import System.Environment
import Web.Scotty

import Language.Haskell.Nemnem.Parse.Module
import Language.Haskell.Nemnem.Parse
import Language.Haskell.Nemnem.Printer

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
main = do
  args <- getArgs
  -- in DAG order (no recursive module support - yet?)
  let default_paths =
        [ "tsrc/DataTextInternal.hs"
        , "tsrc/DataText.hs"
        , "tsrc/DataTextIO.hs"
        , "tsrc/Test4.hs"
        , "tsrc/Test3.hs"
        , "src/Language/Haskell/Nemnem/Util.hs"
        , "src/Language/Haskell/Nemnem/Parser.hs"
        , "src/Hier.hs"
        , "src/Language/Haskell/Nemnem/Printer.hs"
        ]
      outdir = "deploy/"
  paths <- case args of
    [] -> return default_paths
    (path_file:_) -> filter (not . ("#" `L.isPrefixOf`)) . lines <$>
                       readFile path_file
  module_infos <- mapM_ (processModules outdir) paths `execStateT` M.empty
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
  where
  hoistMaybe = MaybeT . return
  processModules :: FilePath  -- ^ Directory to output highlit source to
                 -> FilePath  -- ^ Module source location
                 -> StateT (Map MName ModuleInfo) IO ()
  processModules outdir path = do
    raw_src <- lift $ readFile path
    err_or_mi <- processModule (path, raw_src)
    case err_or_mi of
      Left err -> lift $ print err
      Right (parsed_src, mi) -> lift $ do
        print . miName $ mi
        writeLinked outdir parsed_src mi
  --
  writeLinked :: String -> String -> ModuleInfo -> IO ()
  writeLinked outdir src mi =
    TL.writeFile (outdir ++ fromMaybe "Anonymous" (miName mi) ++ ".html") $
      renderTaggedHtml moduleTransformStatic src mi  
  --
  renderTaggedHtml :: (Maybe MName -> TL.Text) -> String -> ModuleInfo -> TL.Text
  renderTaggedHtml module_transform src mi =
    -- assumes newline is \n (single char)
    let lineLens = map ((+1) . length) (lines src)
        bases = basesOf (miRefs mi) (miExports mi)
        ranges = let ofsFun = offsetAt (mkOffsetTable lineLens)
                     refs = map (refToRange ofsFun) (miRefs mi)
                     entities =
                       mapMaybe (tagEntitiesOfCurrentModule ofsFun (miName mi))
                                bases
                     warns = map (warnsToRange ofsFun) (miWarns mi)
                     hlits = map (highlightsToRange ofsFun) (miHighlights mi)
                 in DL.toList . DL.concat . map DL.fromList $
                      [refs, entities, warns, hlits]
        tagged = tagRanges (tagToBlaze module_transform (miName mi))
                           ranges (T.pack src)
    in BR.renderHtml . withHeader $ tagged
  break = putStrLn "---"
  putStrBreak x = break >> putStrLn x

-- TODO these methods rather belong to nemnem-lib/.../Printer.hs
(Range _ a b) `contains` (Range _ c d) = a <= c && d <= b
modifyR f (Range t a b) = Range (f t) a b
getR (Range t _ _) = t
startR (Range _ s _) = s
endR (Range _ _ e) = e

-- Note: could check if using Text.Lazy could lower the memory usage
-- TODO this can blow up if the ranges are messed up, do some checking
tagRanges
  :: (a -> B.Markup -> B.Markup)
  -> [Range a]
  -> T.Text
  -> B.Markup
tagRanges blazer rs txt0 =
  let fs = map (modifyR blazer) rs
      ordered = L.sortBy (comparing (\(Range _ s e) -> (s,-e))) fs
      tlen = T.length txt0
      full = Range undefined 0 tlen
  in go ((0, txt0), [(mempty, full)]) ordered
  where
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

  gapAndRest gap_size txt =
    let (gap, txt1) = T.splitAt gap_size txt
        blaze_gap = if T.null gap then mempty else toBlaze gap
    in (blaze_gap, txt1)
