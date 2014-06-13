{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes,
             OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (foldM_)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text.Lazy.IO as TIO
import qualified Text.Blaze.Html.Renderer.Text as BR
import Hier
import Language.Haskell.Exts.Annotated

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
      path1 = "tsrc/Test4.hs"
      path2 = "tsrc/Test3.hs"
      outdir = "deploy/"
  foldM_ (processModule outdir) M.empty paths
  where
  processModule outdir modules path = do
    src <- uncpp <$> readFile path  -- TODO use text
    let ast = fromParseResult . parse $ src
        mi = collectModule modules ast
        -- switch of fixities - this results in possibly incorrect AST, but
        -- on the source highlighting / indexing level we don't care about
        -- larger expressions anyway.
        parse = parseFileContentsWithMode (defaultParseMode {fixities = Nothing})
    print . miName $ mi
    putStrLn "====exports===="
    print . miExports $ mi
    putStrLn "====warns====="
    putStrLn . prettish 0 . show .  miWarns $ mi
    writeLinked outdir src mi
    return (M.insert (fromMaybe "anonymous" . miName $ mi) mi modules)
  --
  uncpp = unlines . map replace_cpp . lines
  replace_cpp l =
    if "#" `L.isPrefixOf` l
    then "-- " ++ l ++ " !!NOT COMMENTED IN ORIGINAL!!"
    else l
  --
  writeLinked :: String -> String -> ModuleInfo -> IO ()
  writeLinked outdir src mi =
    -- assumes newline is \n (single char)
    let lineLens = map ((+1) . length) (lines src)
        bases = basesOf (miRefs mi)
        ranges = map (refToRange lineLens) (miRefs mi) ++
                   mapMaybe (tagEntitiesOfCurrentModule lineLens (miName mi)) bases
    in do
      let tagged = tagRegions ranges src
      TIO.writeFile (outdir ++ fromMaybe "anonymous" (miName mi) ++ ".html") $
        (BR.renderHtml . withHeader . untag (tagToBlaze$ miName mi) . fmap toBlaze)
          tagged
      --mapM_ (putStrBreak . show) ranges
      --putStrBreak$ prettish 0$ show tagged
  break = putStrLn "---"
  putStrBreak x = break >> putStrLn x


