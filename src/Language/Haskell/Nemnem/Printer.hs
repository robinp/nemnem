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
data Tag = LinkTo (Maybe MName) Text  -- ^ Module and location name
         | LineEnd
         | Entity Text  -- ^ Location name (implicitly local)
         | Warning Text
         | HighlightClass Text
  deriving Show

tagToBlaze :: (Maybe MName -> Text)  -- ^ How to get url path from module
           -> Maybe MName -> Tag -> B.Markup -> B.Markup
tagToBlaze module_to_path current_module t = case t of
  LinkTo link_module location ->
    if current_module == link_module
    then BH.a
           ! BA.href (B.toValue$ "#" <> location)
           ! highlightOnHover True location
    else let mname = module_to_path link_module
             href = mname <> "#" <> location
         in BH.a
              ! BA.href (B.toValue href)
              ! highlightOnHover False href
  LineEnd -> const BH.br
  Entity location -> 
    BH.a
      ! BA.name (B.toValue location)
      ! highlightOnHover True location
  Warning txt ->
    BH.span
      ! BA.class_ "warning"
      ! BH.dataAttribute "warning" (B.toValue txt)
  HighlightClass cls ->
    BH.span ! BA.class_ (B.toValue cls)
  where
  highlightOnHover is_local arg =
    let function = if is_local
          then "highlightLocalToLocal"
          else "highlightLocalToRemote"
    in BA.onmouseover . B.toValue . mconcat $
         ["nemnem.", function, "('", arg, "')"]

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
refToRange lineLens (Ref (SymLoc (srcStart, srcEnd) _) dst stack) =
  mkRange (LinkTo (symModule dst) (idfy dst))
          (lc srcStart) (lc srcEnd)
  where
  lc = lineAndColumnToOffset lineLens

warnsToRange :: [Int] -> Warn -> TaggedRange String Tag
warnsToRange line_lens (Warn (start, end) w_txt) =
  mkRange (Warning . T.pack $ w_txt) (lc start) (lc end)
  where
  lc = lineAndColumnToOffset line_lens

highlightsToRange :: [Int] -> Highlight -> TaggedRange String Tag
highlightsToRange line_lens (Highlight (start, end) hl_kind) =
  mkRange (HighlightClass . T.pack $ cls) (lc start) (lc end)
  where
  lc = lineAndColumnToOffset line_lens
  cls = case hl_kind of
          Literal CharLit -> "hl_char"
          Literal StringLit -> "hl_string"
          Literal NumLit -> "hl_num"
          VariableDecl OtherVarDecl -> "hl_vardecl"
          VariableDecl MatchHead -> "hl_fundecl"  -- imprecise name
          VariableDecl RecordField -> "hl_fielddecl"
          TypeConstructorDecl -> "hl_tycondecl"
          TypeVariable -> "hl_tyvar"
          TypeConstructorRef -> "hl_tyconref"
          SpecialName -> "hl_specname"
          Infix -> "hl_infix"
          VariableRef -> "hl_varref"
          ConstructorDecl -> "hl_condecl"
          ConstructorRef -> "hl_conref"
          ClassRef -> "hl_classref"
          CommentHL -> "hl_comment"
          Pragma -> "hl_pragma"

tagEntitiesOfCurrentModule :: [Int] -> Maybe MName -> SymLoc -> Maybe (TaggedRange String Tag)
tagEntitiesOfCurrentModule lineLens curModule sym@(SymLoc (s,e) sModule) =
  if sModule == curModule
    then Just $ mkRange (Entity$ idfy sym) (lc s) (lc e)
    else Nothing
  where lc = lineAndColumnToOffset lineLens

idfy :: SymLoc -> Text
idfy s = 
  "loc_" <> tshow (fst$a$s) <> "_" <> tshow (snd$a$s)
    <> "_" <> tshow (fst$b$s) <> "_" <> tshow (snd$b$s)
  where
  a = fst . symRange
  b = snd . symRange
  tshow = T.pack . show

-- the base is the destination of a reference
basesOf :: [Ref] -> [SymLoc]
basesOf = nub . map (\(Ref _ dst _) -> dst)


