{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
module Language.Haskell.Nemnem.Printer where

import Text.Blaze ((!))
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as BA
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Vector.Unboxed as UV

import Language.Haskell.Nemnem.Parse.Module
import Language.Haskell.Nemnem.Internal.Util

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
    BH.link ! BA.rel "stylesheet" ! BA.type_ "text/css" ! BA.href "static/nemnem.css"
    BH.script ! BA.src "static/jquery-2.0.3.min.js" $ mempty
    BH.script ! BA.src "static/nemnem.js" $ mempty
  BH.body $
    BH.div ! BA.id "code" $ m

toBlaze :: B.ToMarkup a => a -> B.Markup
toBlaze = preStyle . B.toMarkup
preStyle = BH.span ! BA.class_ "mpre"
-----------

type OffsetTable = UV.Vector Int
type OffsetFun = (Int, Int) -> Int

mkOffsetTable :: [Int] -> OffsetTable
mkOffsetTable line_lens = UV.scanl' (+) 0 (UV.fromList line_lens)

offsetAt :: OffsetTable -> OffsetFun
offsetAt table (line, col) = table UV.! (line - 1) + col - 1

data Range a = Range a !Int !Int

refToRange :: OffsetFun -> Ref -> Range Tag
refToRange lc (Ref (SymLoc (srcStart, srcEnd) _) dst stack) =
  Range (LinkTo (symModule dst) (idfy dst))
        (lc srcStart) (lc srcEnd)

warnsToRange :: OffsetFun -> Warn -> Range Tag
warnsToRange lc (Warn (start, end) w_txt) =
  Range (Warning . T.pack $ w_txt) (lc start) (lc end)

highlightsToRange :: OffsetFun -> Highlight -> Range Tag
highlightsToRange lc (Highlight (start, end) hl_kind) =
  Range (HighlightClass cls) (lc start) (lc end)
  where
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
          ClassDeclHL -> "hl_classdecl"
          ClassRef -> "hl_classref"
          CommentHL -> "hl_comment"
          Pragma -> "hl_pragma"

tagEntitiesOfCurrentModule
  :: OffsetFun -> Maybe MName -> SymLoc -> Maybe (Range Tag)
tagEntitiesOfCurrentModule lc curModule sym@(SymLoc (s,e) sModule) =
  if sModule == curModule
    then Just $ Range (Entity$ idfy sym) (lc s) (lc e)
    else Nothing

idfy :: SymLoc -> Text
idfy s = 
  "loc_" <> tshow (fst$a$s) <> "_" <> tshow (snd$a$s)
    <> "_" <> tshow (fst$b$s) <> "_" <> tshow (snd$b$s)
  where
  a = fst . symRange
  b = snd . symRange
  tshow = T.pack . show

-- | The base is the destination of a reference.
-- Exported symbols are also included, since these are potentially referenced
-- in foreign modules.
basesOf :: [Ref] -> SymTab -> [SymLoc]
basesOf refs exported_syms =
  let ref_targets = map (\(Ref _ dst _) -> dst) refs
      exported_locs = M.elems $ exported_syms
  in nub $ nub ref_targets ++ exported_locs


