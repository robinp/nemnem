{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes,
             OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.Trans.RWS
import Data.List (intersperse, nub, inits)
import Data.Map (Map)
import Data.Maybe (maybeToList, fromMaybe, catMaybes)
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
import qualified Data.Text.Lazy.IO as TIO

import Hier

type MName = String

--linkFile :: StateT SymTab

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
  where spaces x = take x $ repeat ' '

main = do
  let path1 = "tsrc/Test4.hs"
  let path2 = "tsrc/Test3.hs"
  let outdir = "deploy/"
  src1 <- readFile path1
  src2 <- readFile path2
  ast1 <- fromParseResult <$> parseFile path1
  ast2 <- fromParseResult <$> parseFile path2
  putStrBreak . ("AST " ++) . prettish 0 . show $ fmap (const "") ast2
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
                     catMaybes (map (baseToRange lineLens (miName mi)) bases)
      in do
        let tagged = tagRegions ranges src
        TIO.writeFile (outdir ++ (maybe "anonymous" id (miName mi)) ++ ".html") $
          (BR.renderHtml . withHeader . untag (tagToBlaze$ miName mi) . fmap toBlaze)
            tagged
        mapM_ (putStrBreak . show) ranges
        putStrBreak$ prettish 0$ show tagged
    break = putStrLn "---"
    putStrBreak x = break >> putStrLn x

instance Show (TaggedRange String Tag) where
  show = showTaggedRange

----------- Blaze stuff
data Tag = LinkTo (Maybe MName) Text
         | LineEnd
         | Entity Text
  deriving Show

cond c a b = if c then a else b

tagToBlaze :: Maybe MName -> Tag -> B.Markup -> B.Markup
tagToBlaze mname t = case t of
  LinkTo mod ref ->
    cond (mod == mname || mod == Nothing) (! hoverAttrib) id $
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
type LineColRange = (LineCol,LineCol)

refToRange :: [Int] -> Ref -> TaggedRange String Tag
refToRange lineLens (Ref (SymV (srcStart, srcEnd) _) dst) =
  mkRange (LinkTo (symModule dst) (idfy dst)) (lc srcStart) (lc srcEnd)
  where lc = mapLineCol lineLens

baseToRange :: [Int] -> Maybe MName -> SymV -> Maybe (TaggedRange String Tag)
baseToRange lineLens curModule sym@(SymV (s,e) sModule) =
  -- sModule is Nothing is a local var? TODO explain
  if sModule == curModule || sModule == Nothing
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

---
data Ctx a = CType a | CTerm a
  deriving (Eq, Ord, Show)

instance Functor Ctx where
  fmap f (CType a) = CType (f a)
  fmap f (CTerm a) = CTerm (f a)

type S = SrcSpanInfo

type SymK = Ctx String
data SymV = SymV
  { symRange :: LineColRange
  , symModule :: Maybe String
  } deriving (Eq, Show)
type SymKV = (SymK, SymV)
type SymTab = Map SymK SymV

wrapLoc :: S -> SymV
wrapLoc loc = SymV { symRange = lineCol loc, symModule = Nothing }

data Ref = Ref
  { refSource :: SymV
  , refTarget :: SymV
  } deriving Show

getRef (LRef r) = [r]
getRef _ = []
getWarn (LWarn s) = [s]
getWarn _ = []
getChild (LChild p c) = [(p, c)]
getChild _ = []

lineCol :: S -> LineColRange
lineCol src = 
  let s = srcInfoSpan src
  in ((srcSpanStartLine s, srcSpanStartColumn s),
      (srcSpanEndLine s, srcSpanEndColumn s))

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

-- * Link collector part

data Log = LRef Ref | LWarn String | LChild (Ctx String) (Ctx String)
  deriving Show
type Logs = [Log]

type Collector = ([SymKV], SymTab -> Logs)

data ModuleInfo = ModuleInfo
  { miName :: Maybe MName
    -- newly defined top-level symbols (may not be exported)
  , miSymbols :: SymTab
    -- exported symbols
  , miExports :: SymTab
  , miChildren :: ChildMap
    -- references from this module
  , miRefs :: [Ref]
    -- warnings while processing AST (likely todos)
  , miWarns :: [String]
  }

type ChildMap = Map SymK [SymK]

exportedKeys :: ChildMap -> [ExportSpec l] -> [Ctx String]
exportedKeys children xs = xs >>= exports
  where
    exports x = case x of
      EVar _ qname -> getQName CTerm qname
      EAbs _ qname -> getQName CType qname
      EThingAll _ qname -> do
        name <- getQName CType qname
        name:(M.findWithDefault [] name children)
      EThingWith _ qname cnames ->
        getQName CType qname ++ map (CTerm . getCName) cnames
      EModuleContents _ (ModuleName _ mname) -> error "EModuleContents"
    getQName ctx =
      -- TODO maybe should drop module part instead flatten? what's the
      --    spec here?
      maybeToList . fmap (ctx . snd) . flattenQName

getCName (VarName _ name) = nameVal name
getCName (ConName _ name) = nameVal name

imports :: ChildMap -> ImportSpec S -> [SymK]
imports children is = case is of
  IVar _ name -> getName CTerm name
  IAbs _ name ->
    -- See Haskell report 5.3.1 item 2
    getName CType name ++ getName CTerm name
  IThingAll _ name -> do
    n <- getName CType name
    n:(M.findWithDefault [] n children)
  IThingWith _ name cnames ->
    getName CType name ++ map (CTerm . getCName) cnames
  where
    getName :: (forall a . a -> Ctx a) -> Name S -> [SymK]
    getName ctx = (\x -> [x])  . fst . mkNameSym ctx

importSyms :: Map MName ModuleInfo -> [ImportDecl S] -> SymTab
importSyms modules = mconcat . map getImports
  where
    getImports (ImportDecl _l lmname isQual _src _pkg alias specs) =
      fromMaybe M.empty $ for (M.lookup (mname lmname) modules) (\mi ->
      let exports = miExports mi
          filtered = maybe id (filterImports$ miChildren mi) specs exports
          aliased mAlias =
            let prefix = mAlias ++ "."
            in M.mapKeys (fmap (prefix ++)) filtered
      in case alias of
           Just a -> aliased (mname a) `mappend`
                       cond isQual M.empty filtered
           Nothing -> cond isQual (aliased$ mname lmname) filtered
      )
    mname (ModuleName _ m) = m
    filterImports :: ChildMap -> ImportSpecList S -> SymTab -> SymTab
    filterImports children (ImportSpecList _ isHiding iss) syms =
      let selected = iss >>= imports children -- TODO use set
          passes s = invertIf isHiding $ s `elem` selected
      in M.filterWithKey (\s _ -> passes s) syms

invertIf :: Bool -> Bool -> Bool
invertIf f = cond f not id

collectModule :: Map MName ModuleInfo -> Module S -> ModuleInfo
collectModule modules (Module _l mhead _pragmas imports decls) =
  let (syms, funs) = unzip $ map collectDecl decls
      mname = moduleName <$> mhead
      symTab = M.map (\s -> s { symModule = mname }) $
                 M.fromList$ concat syms
      logs = mergeCollect funs
               (symTab `mappend` (importSyms modules imports))
               ++ [LWarn . ("IMPORTS " ++ ) . show $ importSyms modules imports]
      children =
        let pairs = map (\(p,c) -> (p, [c])) $ logs >>= getChild
        in M.unionsWith (++) (map (M.fromList . return) pairs)
      exports = fromMaybe M.empty $ headExports symTab children <$> mhead
  in ModuleInfo
      { miName = mname
      , miSymbols = symTab
      , miExports = exports
      , miChildren = exportedChildren exports children
      , miRefs = logs >>= getRef
      , miWarns = logs >>= getWarn
      }
  where
    moduleName (ModuleHead _ (ModuleName _ mname) _ _) = mname
    headExports symTab children (ModuleHead _ _ _ exportSpecs) =
      case exportSpecs of
        Nothing -> symTab
        Just (ExportSpecList _ xs) ->
          let keySet = exportedKeys children xs
             -- TODO improve lookup efficiency
          in M.filterWithKey (\k _ -> k `elem` keySet) symTab
    exportedChildren :: SymTab -> (ChildMap -> ChildMap)
    exportedChildren exports =
      let exported = flip M.member exports
      in M.map (filter exported) . M.filterWithKey (\s _ -> exported s)

mergeCollect :: [SymTab -> Logs] -> SymTab -> Logs
mergeCollect fs s = concat $ map ($ s) fs

collectDecl :: Decl S -> Collector
collectDecl decl = case decl of
  DataDecl _l _dataOrNew _ctx dHead quals derivings ->
    let headSym@(headCtx,_) = collectDeclHead dHead
        (qualSyms, qualFuns) = unzip $ map (collectQualConDecl headCtx) quals
    in (headSym:(concat qualSyms), mergeCollect qualFuns)
  TypeSig _l names ty ->
    -- we want names in signatures to point to the definitions
    let nameFuns = map fname names
        tyFun = collectType ty
        fname name s = maybeToList$
          LRef . Ref (namePos name) <$> M.lookup (CTerm$ nameVal name) s
    in ([], mergeCollect$ tyFun:nameFuns)
  FunBind _l matches ->
    let (syms, funs) = unzip $ for matches (\m ->
          let (name, pats, rhs, binds) = case m of
                Match _l nm ps r bs -> (nm, ps, r, bs)
                InfixMatch _l p nm ps r bs -> (nm, p:ps, r, bs)
              matchSym = mkNameSym CTerm name
              (patSyms, patFuns) = unzip $ map collectPat pats
              -- patSyms are not returned to the global scope, just
              -- used when resolving rhs and binds
              -- TODO rhs, binds
              rhsFun = \s -> collectRhs rhs (M.fromList (concat patSyms) `M.union` s)
          in ([matchSym], mergeCollect$ rhsFun:patFuns))
    in (concat syms, mergeCollect funs)
  -- TODO
  other -> ([], const [LWarn$ "xDECL " ++ show other])

collectRhs :: Rhs S -> SymTab -> Logs
collectRhs rhs = case rhs of
  UnGuardedRhs _l exp -> collectExp exp
  other -> const [LWarn$ "GuardedRhs " ++ show other]

collectExp :: Exp S -> SymTab -> Logs
collectExp exp = case exp of
  Var _l qname -> resolveQName CTerm qname
  Con _l qname -> resolveQName CTerm qname
  InfixApp _l lexp _op rexp -> exps [lexp, rexp]
  App _l exp1 exp2 -> exps [exp1, exp2]
  Case _l exp alts -> (\s ->
    let expLogs = collectExp exp s
        altLogs = mergeCollect (map collectAlt alts) s
    in expLogs ++ altLogs)
  other -> const [LWarn$ "Exp " ++ show exp]
  where
    exps ee = mergeCollect$ map collectExp ee

collectAlt :: Alt S -> SymTab -> Logs
collectAlt (Alt _l pat guardedAlts binds) =
  let (patSyms, patFun) = collectPat pat
      -- patSyms only used in guardedAlts
      altFun = \s -> collectGuardedAlts guardedAlts (M.fromList patSyms `M.union` s)
  in mergeCollect [patFun, altFun]

collectGuardedAlts :: GuardedAlts S -> SymTab -> Logs
collectGuardedAlts gas = case gas of
  UnGuardedAlt _l exp -> collectExp exp
  other -> const [LWarn$ "Guarded case alternative: " ++ show other]

collectPat :: Pat S -> Collector
collectPat p = case p of
  PVar _l name -> (return$ mkNameSym CTerm name, const [])
  PApp _l qname pats ->
    let qnameFun = resolveQName CTerm qname
        (psyms, pfuns) = unzip $ map collectPat pats
    in (concat psyms, mergeCollect (qnameFun:pfuns))
  PParen _l pat -> collectPat pat
  other -> ([], const [LWarn$ "xPat" ++ show other])

collectDeclHead :: DeclHead S -> SymKV
collectDeclHead dhead = case dhead of
  DHead _hl name _tyvarbinds -> mkNameSym CType name
  DHInfix _hl _tyleft name _tyright -> mkNameSym CType name
  DHParen _hl innerHead -> collectDeclHead innerHead

mkNameSym :: (forall a. a -> Ctx a) -> Name S -> SymKV
mkNameSym ctxType name = (ctxType (nameVal name), namePos name)

mkChild :: Ctx String -> (forall a. a -> Ctx a) -> Name S -> a -> Logs
mkChild p ctx c = const [LChild p (ctx$ nameVal c)]

collectQualConDecl :: Ctx String -> QualConDecl S -> Collector
collectQualConDecl tyname (QualConDecl _l _tyvarbinds ctx conDecl) =
  -- TODO ctx
  case conDecl of
    ConDecl _l name bangs -> ctorBangs name bangs
    InfixConDecl _l lbang name rbang -> ctorBangs name [lbang, rbang]
    RecDecl _l name fields ->
      let ctorSym = mkNameSym CTerm name
          (fieldSyms, fs) = unzip $ map collectFieldDecl fields
      in (ctorSym:(concat fieldSyms), mergeCollect fs)
  where
    bangCollect = mergeCollect . map (collectType . bangType)
    ctorBangs name bangs =
      ([mkNameSym CTerm name],
        mergeCollect [mkChild tyname CTerm name, bangCollect bangs])

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
  TyCon _l qname -> resolveQName CType qname st -- ++ 
                      --[LWarn$ prettish 5 $ "xTTT" ++ show (hush qname) ++ "|| " ++ show st]
  TyFun _l t1 t2 -> mergeCollect (map collectType [t1, t2]) st
  other -> [LWarn$ "xTyCon " ++ show ty]
  --where hush = fmap (const "")

resolveQName :: (forall a. a -> Ctx a) -> QName S -> SymTab -> Logs
resolveQName ct qname s = maybeToList $ do
  (l, name) <- flattenQName qname
  refPos <- M.lookup (ct name) s
  return$ LRef (Ref (wrapLoc l) refPos)

flattenQName :: QName l -> Maybe (l, String)
flattenQName qname = case qname of
  -- TODO does this impl fit all uses?
  Qual l (ModuleName _ mname) name -> Just (l, mname ++ "." ++ nameVal name)
  UnQual l name -> Just (l, nameVal name)
  Special _ _ -> Nothing

namePos (Symbol p _) = wrapLoc p
namePos (Ident p _) = wrapLoc p

nameVal (Symbol _ v) = v
nameVal (Ident _ v) = v
