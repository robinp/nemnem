{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes #-}
module Language.Haskell.Nemnem.Parser where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.Identity
import Control.Monad.Trans.RWS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Language.Haskell.Exts.Annotated

import Language.Haskell.Nemnem.Util

data ParseCtx = ParseCtx
  { inScope :: SymTab  -- ^ Tied recursively.
  , parseOpts :: ()
  } 

type ParseT m a = RWST ParseCtx Logs () m a
type Parse a = ParseT Identity a

type MName = String

data Ctx = CType | CTerm
  deriving (Eq, Ord, Show)

data Ctxed a = Ctxed
  { ctx :: Ctx
  , dropCtx :: a
  }
  deriving (Eq, Ord, Show)

instance Functor Ctxed where
  fmap f (Ctxed c a) = Ctxed c (f a)

type S = SrcSpanInfo

type LineCol = (Int,Int)
type LineColRange = (LineCol,LineCol)

type Symbol = Ctxed String
data SymLoc = SymLoc
  { symRange :: LineColRange
  , symModule :: Maybe String  -- TODO these Strings are ugly
  }
  deriving (Eq, Show)

type SymbolAndLoc = (Symbol, SymLoc)
type SymTab = Map Symbol SymLoc

-- Ref is now suitable with generating visual hyperrefs, but not much for
-- searching / indexing / more intelligent displays. 
-- TODO should rather be SymbolAndLoc? 
data Ref = Ref
  { refSource :: SymLoc
  , refTarget :: SymLoc
  }
  deriving Show

getRef (LRef r) = [r]
getRef _ = []
getWarn (LWarn s) = [s]
getWarn _ = []
getChild (LChild p c) = [(p, c)]
getChild _ = []
getAnnotation (LAnotate l s) = [(l, s)]
getAnnotation _ = []

lineCol :: S -> LineColRange
lineCol src = 
  let s = srcInfoSpan src
  in ((srcSpanStartLine s, srcSpanStartColumn s),
      (srcSpanEndLine s, srcSpanEndColumn s))

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

-- * Link collector part

-- TODO add module references, where imported module name points to module
--      source
data Log
  = LRef Ref
  | LWarn String
  | LChild (Ctxed String) (Ctxed String)
  | LAnotate SymLoc String  -- for visual debugging
  deriving Show

type Logs = [Log]

type Collector = ([SymbolAndLoc], SymTab -> Logs)
type ChildMap = Map Symbol [Symbol]

data ModuleInfo = ModuleInfo
  { miName :: Maybe MName
    -- | newly defined top-level symbols (regardless of being exported or not)
  , miSymbols :: SymTab
    -- | exported symbols
  , miExports :: SymTab
    -- | exported parent-child relations (for example type -> constructors)
  , miChildren :: ChildMap
    -- | references from this module
  , miRefs :: [Ref]
    -- | warnings while processing AST (likely todos)
  , miWarns :: [String]
  }

exportedKeys :: ChildMap -> [ExportSpec l] -> [Ctxed String]
exportedKeys children xs = xs >>= exports
  where
    exports x = case x of
      EVar _ qname -> getQName CTerm qname
      EAbs _ qname -> getQName CType qname
      EThingAll _ qname -> do
        name <- getQName CType qname
        name : M.findWithDefault [] name children
      EThingWith _ qname cnames ->
        getQName CType qname ++ map (Ctxed CTerm . getCName) cnames
      EModuleContents _ (ModuleName _ mname) -> error "TODO EModuleContents"
    getQName ctx =
      -- TODO maybe should drop module part instead flatten? what's the
      --    spec here?
      maybeToList . fmap (Ctxed ctx . snd) . flattenQName

getCName (VarName _ name) = hseSymOrIdentValue name
getCName (ConName _ name) = hseSymOrIdentValue name

getCPos (VarName l _) = wrapLoc l
getCPos (ConName l _) = wrapLoc l

-- | Resolves imports specified by ImportSpec.
--
--  The ChildMap is that of the imported module, and is needed to resolve
--  the `Thing(..)` style imports.
--
--  Returns the symbols imported from that module, along with reference
--  locations inside the ImportSpec (not present for the (..) scheme).
imports :: ChildMap
        -> ImportSpec S
        -> [(Symbol, Maybe SymLoc)]
imports children is = case is of
  IVar _ name -> justSeconds [hseNameToSymbolAndLoc CTerm name]
  IAbs _ name ->
    -- See Haskell report 5.3.1 item 2
    justSeconds $ 
      [hseNameToSymbolAndLoc CType name, hseNameToSymbolAndLoc CTerm name]
  IThingAll _ name ->
    let n = hseNameToSymbolAndLoc CType name
    in mapSnd Just n :
         map (flip (,) Nothing) (M.findWithDefault [] (fst n) children)
  IThingWith _ name cnames ->
    mapSnd Just (hseNameToSymbolAndLoc CType name) : 
      map (\cn -> (Ctxed CTerm . getCName $ cn, Just . getCPos $ cn)) cnames
  where
  justSeconds = map (mapSnd Just)

importSyms :: Map MName ModuleInfo -> [ImportDecl S] -> Parse SymTab
importSyms modules decls = mconcat <$> mapM getImports decls
  where
  getImports (ImportDecl _l lmname isQual _src _pkg alias mb_specs) =
    case M.lookup (mname lmname) modules of
      Nothing -> return mempty  -- Imported module not yet processed?
      Just mi -> do
        filtered <-
          let exports = miExports mi
          in case mb_specs of
            Just specs -> filterImports (miChildren mi) specs exports
            Nothing -> return exports
        return $ case alias of
          Just a -> aliased (mname a) filtered
                    `mappend` if isQual then M.empty else filtered
          Nothing -> applyIf isQual (aliased $ mname lmname) filtered
  --
  applyIf :: Bool -> (a -> a) -> a -> a
  applyIf c f = if c then f else id
  --
  aliased mAlias exps =
    let prefix = mAlias ++ "."
    in M.mapKeys (fmap (prefix ++)) exps
  --
  mname (ModuleName _ m) = m
  --
  filterImports :: ChildMap -> ImportSpecList S -> SymTab -> Parse SymTab
  filterImports children (ImportSpecList _ is_hiding iss) exported_syms =
    let specified = iss >>= imports children  :: [(Symbol, Maybe SymLoc)]
        selected = map fst specified
        passes s = invertIf is_hiding $ s `elem` selected
    in do
      mapM_ (logImport exported_syms) 
        . map (mapSnd fromJust) . filter (isJust . snd) $ specified
      return $ M.filterWithKey (\s _ -> passes s) exported_syms
  --
  logImport :: SymTab -> SymbolAndLoc -> Parse ()
  logImport import_syms (symbol, loc) = case M.lookup symbol import_syms of
    Nothing -> return ()
    Just remote_loc -> tell [LRef $ Ref loc remote_loc]

moduleHeadName (ModuleHead _ (ModuleName _ mname) _ _) = mname

parseModuleSymbols :: Map MName ModuleInfo -> Module S -> Parse SymTab
parseModuleSymbols modules (Module _l mhead _pragmas imports decls) = do
  decl_symtabs <- mapM collectDecl decls
  let mname = moduleHeadName <$> mhead
      module_symtab = M.map (\s -> s { symModule = mname }) $
                        M.unions decl_symtabs
  imported_symtab <- importSyms modules imports
  return $ module_symtab `M.union` imported_symtab

collectModule :: Map MName ModuleInfo -> Module S -> ModuleInfo
collectModule modules m@(Module _l mhead _pragmas imports decls) =
  let (symtab, _, logs) = runRWS (parseModuleSymbols modules m) pctx ()
      pctx = ParseCtx { inScope = symtab, parseOpts = () }
      children =
        let pairs = map (\(p,c) -> (p, [c])) $ logs >>= getChild
        in M.unionsWith (++) (map (M.fromList . return) pairs)
      exports = fromMaybe M.empty $ headExports symtab children <$> mhead
  in ModuleInfo
      { miName = moduleHeadName <$> mhead
      , miSymbols = symtab
      , miExports = exports
      , miChildren = exportedChildren exports children
      , miRefs = logs >>= getRef
      , miWarns = logs >>= getWarn
      }
  where
  headExports symtab children (ModuleHead _ _ _ exportSpecs) =
    case exportSpecs of
      Nothing -> symtab
      Just (ExportSpecList _ xs) ->
        let key_set = exportedKeys children xs
           -- TODO improve lookup efficiency
        in M.filterWithKey (\k _ -> k `elem` key_set) symtab
  exportedChildren :: SymTab -> ChildMap -> ChildMap
  exportedChildren exports =
    let exported = flip M.member exports
    in M.map (filter exported) . M.filterWithKey (\s _ -> exported s)

mergeCollect :: [SymTab -> Logs] -> SymTab -> Logs
mergeCollect fs s = concatMap ($ s) fs

collectDecl :: Decl S -> Parse SymTab
collectDecl decl = do
  syms <- asks inScope
  let (topSymAndLocs, fun) = collectDecl' decl
      subLogs = fun syms
  tell subLogs
  return . M.fromList $ topSymAndLocs

collectDecl' :: Decl S -> Collector
collectDecl' decl = case decl of
  DataDecl _l _dataOrNew _ctx dHead quals derivings ->
    let headSym@(headCtx,_) = collectDeclHead dHead
        (qualSyms, qualFuns) = unzip $ map (collectQualConDecl headCtx) quals
    in (headSym : concat qualSyms, mergeCollect qualFuns)
  TypeSig _l names ty ->
    -- we want names in signatures to point to the definitions
    let nameFuns = map fname names
        tyFun = collectType ty
        fname name s = maybeToList$
          LRef . Ref (hseSymOrIdentPos name) <$> M.lookup (Ctxed CTerm$ hseSymOrIdentValue name) s
    in ([], mergeCollect$ tyFun:nameFuns)
  FunBind _l matches ->
    let (syms, funs) = unzip $ for matches (\m ->
          let (name, pats, rhs, binds) = case m of
                Match _l nm ps r bs -> (nm, ps, r, bs)
                InfixMatch _l p nm ps r bs -> (nm, p:ps, r, bs)
              matchSym = hseNameToSymbolAndLoc CTerm name
              (patSyms, patFuns) = unzip $ map collectPat pats
              -- patSyms are not returned to the global scope, just
              -- used when resolving rhs and binds
              -- TODO rhs, binds
              rhsFun = collectRhs rhs . (M.fromList (concat patSyms) `M.union`)
          in ([matchSym], mergeCollect$ rhsFun:patFuns))
    in (concat syms, mergeCollect funs)
  PatBind _l pat _typeTODO rhs _bindsTODO ->
    let (patSyms, patFuns) = collectPat pat
        -- TODO duplication with above
        rhsFun = collectRhs rhs . (M.fromList patSyms `M.union`)
    in (patSyms, mergeCollect [rhsFun, patFuns])
  -- TODO
  other -> ([], const [LWarn$ "xDECL " ++ show other])

collectRhs :: Rhs S -> SymTab -> Logs
collectRhs rhs = case rhs of
  UnGuardedRhs _l exp -> collectExp exp
  GuardedRhss _l grhss ->
    mergeCollect . map collectExp $ concat (map expsFrom grhss)
  where
  expsFrom (GuardedRhs _l stmts exp) = exp : (stmts >>= stmtExps)
  stmtExps stmt = case stmt of
    Qualifier _l exp -> [exp]
    other -> error $ "GuardedRhs statement " ++ show other

collectExp :: Exp S -> SymTab -> Logs
collectExp exp = case exp of
  Var _l qname -> resolveQName CTerm qname
  Con _l qname -> resolveQName CTerm qname
  InfixApp _l lexp _op rexp -> exps [lexp, rexp]
  App _l exp1 exp2 -> exps [exp1, exp2]
  NegApp _l exp -> collectExp exp
  Case _l exp alts -> \s ->
    let expLogs = collectExp exp s
        altLogs = mergeCollect (map collectAlt alts) s
    in expLogs ++ altLogs
  If _l c a b -> exps [c, a, b]
  Let _l binds exp -> case binds of
    BDecls _l decls ->
      let (dsyms, dfuns) = unzip $ map collectDecl' decls
          syms = concat dsyms
      in (\s -> mergeCollect (collectExp exp:dfuns) $ M.fromList syms `M.union` s)
    IPBinds _l ipbinds -> error "no IPBind support"
  Paren _l exp -> collectExp exp
  --Lambda pats exp -> 
  --  let (psyms, pfuns) = unzip $ map collectPat pats
  --      syms = concat psyms
  other -> const [LWarn$ "Exp " ++ show exp]
  where
    exps ee = mergeCollect$ map collectExp ee

collectAlt :: Alt S -> SymTab -> Logs
collectAlt (Alt _l pat guardedAlts binds) =
  let (patSyms, patFun) = collectPat pat
      -- patSyms only used in guardedAlts
      altFun s = collectGuardedAlts guardedAlts (M.fromList patSyms `M.union` s)
  in mergeCollect [patFun, altFun]

collectGuardedAlts :: GuardedAlts S -> SymTab -> Logs
collectGuardedAlts gas = case gas of
  UnGuardedAlt _l exp -> collectExp exp
  other -> const [LWarn$ "Guarded case alternative: " ++ show other]

collectPat :: Pat S -> Collector
collectPat p = case p of
  PVar _l name -> (return$ hseNameToSymbolAndLoc CTerm name, const [])
  PApp _l qname pats ->
    let qnameFun = resolveQName CTerm qname
        (psyms, pfuns) = unzip $ map collectPat pats
    in (concat psyms, mergeCollect (qnameFun:pfuns))
  PParen _l pat -> collectPat pat
  other -> ([], const [LWarn$ "xPat" ++ show other])

collectDeclHead :: DeclHead S -> SymbolAndLoc
collectDeclHead dhead = case dhead of
  DHead _hl name _tyvarbinds -> hseNameToSymbolAndLoc CType name
  DHInfix _hl _tyleft name _tyright -> hseNameToSymbolAndLoc CType name
  DHParen _hl innerHead -> collectDeclHead innerHead

-- | Parses a constructor declaration.
collectQualConDecl :: Ctxed String -> QualConDecl S -> Collector
collectQualConDecl tyname (QualConDecl _l _tyvarbinds ctx conDecl) =
  -- TODO ctx
  case conDecl of
    ConDecl _l name bangs -> ctorBangs name bangs
    InfixConDecl _l lbang name rbang -> ctorBangs name [lbang, rbang]
    RecDecl _l name fields ->
      let ctorSym = hseNameToSymbolAndLoc CTerm name
          (fieldSyms, fs) = unzip $ map collectFieldDecl fields
          flatSyms = concat fieldSyms
          tyChildren = for flatSyms (mkChild tyname . fst)
      in (ctorSym:flatSyms, mergeCollect (fs ++ tyChildren))
  where
    bangCollect = mergeCollect . map (collectType . bangType)
    ctorBangs name bangs =
      ([hseNameToSymbolAndLoc CTerm name],
        mergeCollect [mkChild tyname (Ctxed CTerm$ hseSymOrIdentValue name),
                      bangCollect bangs])

collectFieldDecl :: FieldDecl S -> Collector
collectFieldDecl (FieldDecl l names bang) =
  let fsyms = map (hseNameToSymbolAndLoc CTerm) names
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

resolveQName :: Ctx -> QName S -> SymTab -> Logs
resolveQName ctx qname s = maybeToList $ do
  (l, name) <- flattenQName qname
  refPos <- M.lookup (Ctxed ctx name) s
  return . LRef $ Ref (wrapLoc l) refPos

flattenQName :: QName l -> Maybe (l, String)
flattenQName qname = case qname of
  -- TODO does this impl fit all uses?
  Qual l (ModuleName _ mname) name ->
    Just (l, mname ++ "." ++ hseSymOrIdentValue name)
  UnQual l name ->
    Just (l, hseSymOrIdentValue name)
  Special _ _ ->
    Nothing

hseSymOrIdentPos (Symbol p _) = wrapLoc p
hseSymOrIdentPos (Ident p _) = wrapLoc p

hseSymOrIdentValue (Symbol _ v) = v
hseSymOrIdentValue (Ident _ v) = v

wrapLoc :: S -> SymLoc
wrapLoc loc = SymLoc { symRange = lineCol loc, symModule = Nothing }

hseNameToSymbolAndLoc :: Ctx -> Name S -> SymbolAndLoc
hseNameToSymbolAndLoc ctx name = (Ctxed ctx (hseSymOrIdentValue name), hseSymOrIdentPos name)

hseNameToSymbol :: Ctx -> Name S -> Symbol
hseNameToSymbol ctx = fst . hseNameToSymbolAndLoc ctx

mkChild :: Ctxed String -> Ctxed String -> a -> Logs
mkChild p c = const [LChild p c]
