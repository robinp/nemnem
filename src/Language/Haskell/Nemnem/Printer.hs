{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
module Language.Haskell.Nemnem.Printer where

import Text.Blaze ((!))
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as BA
import Data.List (nub)
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Hier

import Language.Haskell.Nemnem.Parser
import Language.Haskell.Nemnem.Util

instance Show (TaggedRange String Tag) where
  show = showTaggedRange

----------- Blaze stuff
data Tag = LinkTo (Maybe MName) Text
         | LineEnd
         | Entity Text
  deriving Show

tagToBlaze :: Maybe MName -> Tag -> B.Markup -> B.Markup
tagToBlaze mname t = case t of
  LinkTo mod ref ->
    BH.a
      ! BA.href (B.toValue ref)
      ! hoverAttrib ref
  LineEnd -> const BH.br
  Entity ref -> 
    BH.a
      ! BA.name (B.toValue ref)
      ! hoverAttrib ref
  where
  hoverAttrib ref = BA.onmouseover (B.toValue$
                      mconcat ["nemnem.highlight('", ref, "')"])

withHeader m = BH.html $ do
  BH.head $ do
    BH.title "NemNem"
    BH.link ! BA.rel "stylesheet" ! BA.type_ "text/css" ! BA.href "/static/nemnem.css"
    BH.script ! BA.src "/static/jquery-2.0.3.min.js" $ mempty
    BH.script ! BA.src "/static/nemnem.js" $ mempty
  BH.body $
    BH.div ! BA.id "code" $ m

toBlaze :: B.ToMarkup a => a -> B.Markup
toBlaze = preStyle . B.toMarkup
preStyle = BH.span ! BA.class_ "mpre"
-----------

-- | Line and column numbers assumed from one
lineAndColumnToOffset :: [Int] -> (Int, Int) -> Int
lineAndColumnToOffset lineLens (line, col) =
  sum (take (line-1) lineLens) + (col-1)

refToRange :: [Int] -> Ref -> TaggedRange String Tag
refToRange lineLens (Ref (SymLoc (srcStart, srcEnd) _) dst) =
  mkRange (LinkTo (symModule dst) (idfy dst)) (lc srcStart) (lc srcEnd)
  where lc = lineAndColumnToOffset lineLens

tagEntitiesOfCurrentModule :: [Int] -> Maybe MName -> SymLoc -> Maybe (TaggedRange String Tag)
tagEntitiesOfCurrentModule lineLens curModule sym@(SymLoc (s,e) sModule) =
  if sModule == curModule
    then Just $ mkRange (Entity$ idfy sym) (lc s) (lc e)
    else Nothing
  where lc = lineAndColumnToOffset lineLens

idfy :: SymLoc -> Text
idfy s = T.pack $ maybe "" (++ "#") (symModule s) ++
  "loc_" ++ show (fst$a$s) ++ "_" ++ show (snd$a$s) ++ "_" ++
    show (fst$b$s) ++ "_" ++ show (snd$b$s)
  where a = fst . symRange
        b = snd . symRange

-- the base is the destination of a reference
basesOf :: [Ref] -> [SymLoc]
basesOf = nub . map (\(Ref _ dst) -> dst)


