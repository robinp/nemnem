{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes,
             OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
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
  let path1 = "src/Hier.hs"
  --let path1 = "tsrc/Test4.hs"
  let path2 = "tsrc/Test3.hs"
  let outdir = "deploy/"
  src1 <- readFile path1
  src2 <- readFile path2
  ast1 <- fromParseResult <$> parseFile path1
  ast2 <- fromParseResult <$> parseFile path2
  putStrBreak . ("AST1 " ++) . prettish 0 . show $ fmap (const "") ast1
  putStrBreak . ("AST2 " ++) . prettish 0 . show $ fmap (const "") ast2
  {- let parseMode = defaultParseMode {
        extensions = extensions defaultParseMode ++ fmap EnableExtension [
          TypeFamilies, FlexibleContexts] }
  let ast = fromParseResult $
              parseModuleWithMode parseMode {parseFilename="stdin"} src -}
  let mi1 = collectModule M.empty ast1
  putStrBreak $ "exports1: " ++ show (miExports mi1)
  putStrBreak $ "refs1: " ++ show (miRefs mi1)
  -- TODO add renamed variants of imports
  let mi2 = collectModule (M.insert "Test4" mi1 M.empty) ast2
  putStrBreak $ "exports2: " ++ show (miExports mi2)
  putStrBreak $ "refs2: " ++ show (miRefs mi2)
  mapM_ putStrBreak $ miWarns mi2
  putStrLn "-----------------"
  writeLinked outdir src1 mi1
  writeLinked outdir src2 mi2
  where
    writeLinked outdir src mi =
      -- assumes newline is \n (single char)
      let lineLens = map ((+1) . length) (lines src)
          bases = basesOf (miRefs mi)
          ranges = map (refToRange lineLens) (miRefs mi) ++
                     mapMaybe (baseToRange lineLens (miName mi)) bases
      in do
        let tagged = tagRegions ranges src
        TIO.writeFile (outdir ++ fromMaybe "anonymous" (miName mi) ++ ".html") $
          (BR.renderHtml . withHeader . untag (tagToBlaze$ miName mi) . fmap toBlaze)
            tagged
        --mapM_ (putStrBreak . show) ranges
        --putStrBreak$ prettish 0$ show tagged
    break = putStrLn "---"
    putStrBreak x = break >> putStrLn x


