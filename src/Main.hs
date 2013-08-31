{-# LANGUAGE FlexibleContexts, RankNTypes, OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.Trans.RWS
import Data.List (intersperse, nub, inits)
import Data.Map (Map)
import Data.Maybe (maybeToList)
import qualified Data.Map as M
import Data.Monoid (mempty, mappend, mconcat)
import Language.Haskell.Exts.Annotated
import Text.Blaze ((!))
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as BA
import qualified Text.Blaze.Html.Renderer.Text as BR
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

import Hier

main = do
  src <- getContents
  let parseMode = defaultParseMode {-
        extensions = extensions defaultParseMode ++ fmap EnableExtension [
          StandaloneDeriving, TemplateHaskell] -}
  let ast = fromParseResult $
              parseModuleWithMode parseMode {parseFilename="stdin"} src
  --putStrLn $ prettyPrint ast
  --putStrLn "-----------------"
  let refs = map refout $ filter isRef $ map purify $ collectModule ast
  let bases = basesOf refs
  --mapM_ (\x -> putStrLn "" >> putStrLn (show x)) $ refs
  --putStrLn "-----------------"
  -- assumes newline is \n (single char)
  let lineLens = map ((+1) . length) (lines src)
  let ranges = map (refToRange lineLens) refs ++
        map (baseToRange lineLens) bases
  putStrLn $ T.unpack$ BR.renderHtml $
    withHeader $ untag tagToBlaze $ fmap toBlaze $ tagRegions ranges src

----------- Blaze stuff
data Tag = LinkTo Text
         | LineEnd
         | Entity Text

tagToBlaze :: Tag -> B.Markup -> B.Markup
tagToBlaze t = case t of
  LinkTo ref -> BH.a ! BA.href (B.toValue$ T.pack "#" `mappend` ref)
  LineEnd -> const BH.br
  Entity ref -> BH.a ! BA.name (B.toValue ref)

withHeader m = BH.html $ do
  BH.head $ do
    BH.title "NemNem"
    BH.link ! BA.rel "stylesheet" ! BA.type_ "text/css" ! BA.href "nemnem.css"
  BH.body $ do
    BH.div ! BA.id "code" $ m  

toBlaze = preStyle . B.toMarkup
preStyle = BH.span ! BA.class_ "mpre"
-----------

-- | Line and column numbers assumed from one
mapLineCol :: [Int] -> (Int, Int) -> Int
mapLineCol lineLens (line, col) =
  sum (take (line-1) lineLens) + (col-1)

type LineCol = (Int,Int)
type LCRange = (LineCol,LineCol)
data PLog = PRef HRef | PWarn String
  deriving Show

isRef (PRef _) = True
isRef _ = False

refout (PRef x) = x
refout _ = undefined

refToRange :: [Int] -> HRef -> TaggedRange String Tag
refToRange lineLens (HRef (srcStart, srcEnd) dst) =
  mkRange (LinkTo$ idfy dst) (lc srcStart) (lc srcEnd)
  where lc = mapLineCol lineLens

baseToRange :: [Int] -> LCRange -> TaggedRange String Tag
baseToRange lineLens lcrange@(s,e) =
  mkRange (Entity$ idfy lcrange) (lc s) (lc e)
  where lc = mapLineCol lineLens

idfy ((la,ca), (lb,cb)) = T.pack $
  show la ++ "_" ++ show ca ++ "_" ++ show lb ++ "_" ++ show cb

basesOf refs = nub $ map (\(HRef _ dst) -> dst) refs

---
data HRef = HRef
  { hSrc :: LCRange
  , hDst :: LCRange
  }  deriving Show

purify (LRef (Ref s t)) = PRef$ HRef (lineCol s) (lineCol t)
purify (LWarn s) = PWarn s

lineCol src = 
  let s = srcInfoSpan src
  in ((srcSpanStartLine s, srcSpanStartColumn s),
      (srcSpanEndLine s, srcSpanEndColumn s))

data Ctx a = CType a | CTerm a
  deriving (Eq, Ord)

type S = SrcSpanInfo

type SymElem = (Ctx String, S)
type SymTab = Map (Ctx String) S

data Ref = Ref
  { refSource :: S
  , refTarget :: S
  }
  deriving Show

data Log = LRef Ref | LWarn String
  deriving Show
type Logs = [Log]
type App = RWS () Logs SymTab ()

type Collector = ([SymElem], SymTab -> Logs)

collectModule :: Module S -> Logs
collectModule (Module _l _head _pragmas _imports decls) =
  let (syms, funs) = unzip $ map collectDecl decls
      symTab = M.fromList $ concat syms
  in mergeCollect funs symTab

mergeCollect :: [SymTab -> Logs] -> SymTab -> Logs
mergeCollect fs s = concat $ map ($ s) fs

collectDecl :: Decl S -> Collector
collectDecl decl = case decl of
  DataDecl _l _dataOrNew _ctx dHead quals derivings ->
    let headSym = collectDeclHead dHead
        (qualSyms, qualFuns) = unzip $ map collectQualConDecl quals
    in (headSym:(concat qualSyms), mergeCollect qualFuns)
  TypeSig _l names ty ->
    -- we want names in signatures to point to the definitions
    let nameFuns = map fname names
        tyFun = collectType ty
        fname name s = maybeToList$
          LRef . Ref (namePos name) <$> M.lookup (CTerm$ nameVal name) s
    in ([], mergeCollect$ tyFun:nameFuns)
  -- TODO
  other -> ([], const [LWarn$ "xDECL " ++ show other])

collectDeclHead :: DeclHead S -> SymElem
collectDeclHead dhead = case dhead of
  DHead _hl name _tyvarbinds -> mkNameSym CType name
  DHInfix _hl _tyleft name _tyright -> mkNameSym CType name
  DHParen _hl innerHead -> collectDeclHead innerHead

mkNameSym :: (forall a. a -> Ctx a) -> Name S -> SymElem
mkNameSym ctxType name = (ctxType (nameVal name), namePos name)

collectQualConDecl :: QualConDecl S -> Collector
collectQualConDecl (QualConDecl _l _tyvarbinds ctx conDecl) =
  -- TODO ctx
  case conDecl of
    ConDecl _l name bangs -> ctorBangs name bangs
    InfixConDecl _l lbang name rbang -> ctorBangs name [lbang, rbang]
    RecDecl _l name fields -> 
      let ctorSym = mkNameSym CTerm name
          -- some repetition here w/collectDecl
          (fieldSyms, fs) = unzip $ map collectFieldDecl fields
      in (ctorSym:(concat fieldSyms), mergeCollect fs)
  where
    bangCollect = mergeCollect . map (collectType . bangType)
    ctorBangs name bangs = ([mkNameSym CTerm name], bangCollect bangs)

collectFieldDecl :: FieldDecl S -> Collector
collectFieldDecl (FieldDecl l names bang) =
  let fsyms = map (mkNameSym CTerm) names
      f = collectType (bangType bang)
  in (fsyms, f)

bangType :: BangType l -> Type l
bangType (BangedTy _ t) = t
bangType (UnBangedTy _ t) = t
bangType (UnpackedTy _ t) = t

collectType :: Type S -> SymTab -> Logs
collectType ty st = case ty of
  TyCon l qname -> maybeToList $ do
    tyname <- flattenQName qname
    refPos <- M.lookup (CType tyname) st
    return$ LRef (Ref l refPos)
  TyFun _l t1 t2 ->
    mergeCollect (map collectType [t1, t2]) st
  other -> [LWarn$ "xTyCon " ++ show ty]

flattenQName :: QName l -> Maybe String
flattenQName qname = case qname of
  -- TODO elaborate a little, no modul support now
  Qual _l (ModuleName _ mname) name -> Just (mname ++ "." ++ nameVal name)
  UnQual _l name -> Just (nameVal name)
  Special _ _ -> Nothing

namePos (Symbol p _) = p
namePos (Ident p _) = p

nameVal (Symbol _ v) = v
nameVal (Ident _ v) = v
