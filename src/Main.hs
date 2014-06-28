{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes,
             OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
--import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State (StateT, execStateT)
import Data.Aeson ()
import Data.Functor.Identity
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Text.Blaze.Html.Renderer.Text as BR
import Hier
import Language.Haskell.Exts.Annotated
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
    raw_src <- lift $ readFile path  -- TODO use text
    err_or_mi <- processModule (path, raw_src)
    case err_or_mi of
      Left err -> lift $ print err
      Right mi -> lift $ do
        print . miName $ mi
        writeLinked outdir raw_src mi
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
        bases = basesOf (miRefs mi)
        ranges = map (refToRange lineLens) (miRefs mi)
                  ++ mapMaybe (tagEntitiesOfCurrentModule lineLens (miName mi))
                     bases
                  ++ map (warnsToRange lineLens) (miWarns mi)
                  ++ map (highlightsToRange lineLens) (miHighlights mi)
        tagged = tagRegions ranges src
    in (BR.renderHtml . withHeader . untag (tagToBlaze module_transform (miName mi)) . fmap toBlaze)
          tagged
  break = putStrLn "---"
  putStrBreak x = break >> putStrLn x


