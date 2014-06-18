{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes,
             OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.Aeson ()
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
import Web.Scotty

import Language.Haskell.Nemnem.Parser
import Language.Haskell.Nemnem.Printer

--linkFile :: StateT SymTab

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

main = do
  -- in DAG order (no recursive module support)
  let paths =
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
  module_infos <- foldM (processModule outdir) M.empty paths
  scotty 8080 $ do
    get "/static/:resource" $ do
      -- TODO this is possibly unsecure
      resource_path <- param "resource"
      file $ "deploy/" ++ resource_path  -- TODO
    get "/module/list" $ do
      json . M.keys $ module_infos
    get "/module/source/get/:name" $ do
      module_name <- param "name"
      -- TODO more RPC-ish (EitherT SomeError IO Res) with auto error handling
      mb_result <- lift . runMaybeT $ do
        module_info <- hoistMaybe $ M.lookup module_name module_infos
        src_path <- hoistMaybe $ miOriginalPath module_info
        src <- lift $ readFile src_path 
        return $ renderTaggedHtml src module_info
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
  processModule :: FilePath              -- ^ Directory to output highlit source to
                -> Map MName ModuleInfo  -- ^ Upstream module infos (at least)
                -> FilePath              -- ^ Module source location
                -> IO (Map MName ModuleInfo)  -- ^ Updated with current module
  processModule outdir modules path = do
    src <- uncpp <$> readFile path  -- TODO use text
    let ast = fromParseResult . parse $ src
        mi = (collectModule modules ast) { miOriginalPath = Just path }
        -- switch of fixities - this results in possibly incorrect AST, but
        -- on the source highlighting / indexing level we don't care about
        -- larger expressions anyway.
        parse = parseFileContentsWithMode (defaultParseMode {fixities = Nothing})
    print . miName $ mi
    --putStrLn "====exports===="
    --print . miExports $ mi
    --putStrLn "====warns====="
    --putStrLn . prettish 0 . show .  miWarns $ mi
    writeLinked outdir src mi
    return (M.insert (fromMaybe "anonymous" . miName $ mi) mi modules)
  --
  uncpp = unlines . map replace_cpp . lines
  replace_cpp l =
    if "#" `L.isPrefixOf` l
    then replicate (length l) ' '  -- length-preserving transform
    else l
  --
  writeLinked :: String -> String -> ModuleInfo -> IO ()
  writeLinked outdir src mi =
    TL.writeFile (outdir ++ fromMaybe "anonymous" (miName mi) ++ ".html") $
      renderTaggedHtml src mi  
  --
  renderTaggedHtml :: String -> ModuleInfo -> TL.Text
  renderTaggedHtml src mi =
    -- assumes newline is \n (single char)
    let lineLens = map ((+1) . length) (lines src)
        bases = basesOf (miRefs mi)
        ranges = map (refToRange lineLens) (miRefs mi) ++
                   mapMaybe (tagEntitiesOfCurrentModule lineLens (miName mi)) bases
        tagged = tagRegions ranges src
    in (BR.renderHtml . withHeader . untag (tagToBlaze$ miName mi) . fmap toBlaze)
          tagged
  break = putStrLn "---"
  putStrBreak x = break >> putStrLn x


