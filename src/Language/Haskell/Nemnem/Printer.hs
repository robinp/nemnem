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
    modifyIf (mod == mname || isNothing mod) (! hoverAttrib) $
      BH.a ! BA.href (B.toValue fullRef)
    where
      hoverAttrib = BA.onmouseover (B.toValue$
                      mconcat ["nemnem.highlight('", ref, "')"])
      fullRef = mconcat [maybe mempty (T.pack . (++ ".html")) mod, "#", ref]
  LineEnd -> const BH.br
  Entity ref -> BH.a ! BA.name (B.toValue ref)

withHeader m = BH.html $ do
  BH.head $ do
    BH.title "NemNem"
    BH.link ! BA.rel "stylesheet" ! BA.type_ "text/css" ! BA.href "nemnem.css"
    BH.script ! BA.src "jquery-2.0.3.min.js" $ mempty
    BH.script ! BA.src "nemnem.js" $ mempty
  BH.body $
    BH.div ! BA.id "code" $ m

toBlaze :: B.ToMarkup a => a -> B.Markup
toBlaze = preStyle . B.toMarkup
preStyle = BH.span ! BA.class_ "mpre"
-----------

-- | Line and column numbers assumed from one
mapLineCol :: [Int] -> (Int, Int) -> Int
mapLineCol lineLens (line, col) =
  sum (take (line-1) lineLens) + (col-1)

refToRange :: [Int] -> Ref -> TaggedRange String Tag
refToRange lineLens (Ref (SymV (srcStart, srcEnd) _) dst) =
  mkRange (LinkTo (symModule dst) (idfy dst)) (lc srcStart) (lc srcEnd)
  where lc = mapLineCol lineLens

baseToRange :: [Int] -> Maybe MName -> SymV -> Maybe (TaggedRange String Tag)
baseToRange lineLens curModule sym@(SymV (s,e) sModule) =
  -- sModule is Nothing is a local var? TODO explain
  if sModule == curModule || isNothing sModule
    then Just $ mkRange (Entity$ idfy sym) (lc s) (lc e)
    else Nothing
  where lc = mapLineCol lineLens

idfy :: SymV -> Text
idfy s = T.pack $
  "loc_" ++ show (fst$a$s) ++ "_" ++ show (snd$a$s) ++ "_" ++
    show (fst$b$s) ++ "_" ++ show (snd$b$s)
  where a = fst . symRange
        b = snd . symRange

-- the base is the destination of a reference
basesOf :: [Ref] -> [SymV]
basesOf = nub . map (\(Ref _ dst) -> dst)


