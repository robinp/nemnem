{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes,
             OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.Aeson ()
import qualified Data.Array as A
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
import Text.Regex.TDFA as R
import System.Environment
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

moduleTransformLive = maybe "Anonymouse" TL.pack
moduleTransformStatic = (<> ".html") . moduleTransformLive
main = do
  args <- getArgs
  -- in DAG order (no recursive module support)
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
  module_infos <- foldM (processModule outdir) M.empty paths
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
  processModule :: FilePath              -- ^ Directory to output highlit source to
                -> Map MName ModuleInfo  -- ^ Upstream module infos (at least)
                -> FilePath              -- ^ Module source location
                -> IO (Map MName ModuleInfo)  -- ^ Updated with current module
  processModule outdir modules path = do
    raw_src <- readFile path  -- TODO use text
    let src = uncpp raw_src
    let (ast, comments) = fromParseResult . parse $ src
        -- TODO preprocess comment locations and pass to collectModule, so it can
        --      link comments to definitions.
        mi = (collectModule modules ast) { miOriginalPath = Just path }
        -- switch off fixities - this results in possibly incorrect AST, but
        -- on the source highlighting / indexing level we don't care about
        -- larger expressions anyway.
        parse = parseFileContentsWithComments $
                  defaultParseMode 
                    { fixities = Nothing
                    , parseFilename = path
                    }
    print . miName $ mi
    -- TODO move comment processing from here to collectModule
    let comment_hls = map getCommentHighlight comments
        mi' = mi { miHighlights = miHighlights mi ++ comment_hls }
    writeLinked outdir raw_src mi'
    return (M.insert (fromMaybe "Anonymous" . miName $ mi') mi' modules)
  --
  uncpp = unlines . reverse . trd . foldl replace_cpp (False, False, []) . lines
  trd (_,_,a) = a
  replace_cpp (was_where, inside_def, res) l0 =
    -- TODO emit preprocessor ranges for syntax highlight
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
    removeMacroCall l =
      let matches = matchAll pattern l :: [MatchArray]
      -- Skip imports since an import `SomeModule (XY(a,b,c..))` would match
      in if "import" `L.isPrefixOf` l || null matches then l
         else let offset_and_lengths = concat . map (A.elems) $ matches
                  regions = map (\(off,len) -> mkRange () off (off+len))
                              offset_and_lengths
              in transformRegions (\_ e -> replicate (length e) ' ') regions l
      where
      pattern = makeRegex ("[A-Z][A-Z0-9_]+\\([^)]*\\)" :: String)  :: Regex
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


