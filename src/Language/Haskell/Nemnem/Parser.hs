{-# LANGUAGE RecursiveDo, FlexibleContexts, FlexibleInstances, RankNTypes #-}
-- TODO remove this once HSE 0.15.0.2 is out
-- See https://github.com/haskell-suite/haskell-src-exts/issues/42
{-# LANGUAGE Arrows #-} 
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

withLocalSymtab :: SymTab -> ParseCtx -> ParseCtx
withLocalSymtab symtab pctx =
  -- M.union is left-biased, which is what we want here.
  pctx { inScope = symtab `M.union` inScope pctx } 

setSymtab :: SymTab -> ParseCtx -> ParseCtx
setSymtab symtab pctx = pctx { inScope = symtab }

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
  | LChild Symbol Symbol
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

exportedKeys :: ChildMap -> [ExportSpec l] -> [Symbol]
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

-- TODO remove
mergeCollect :: [SymTab -> Logs] -> SymTab -> Logs
mergeCollect fs s = concatMap ($ s) fs

collectDecl :: Decl S -> Parse SymTab
collectDecl decl = case decl of
  DataDecl _l _dataOrNew _ctx dHead quals derivings -> do
    -- TODO derivings
    let head_symloc@(head_sym, _) = collectDeclHead dHead
    ctor_symtab <- M.unions <$> mapM (collectQualConDecl head_sym) quals
    return $ insertPair head_symloc ctor_symtab

  TypeSig _l names ty -> do
    -- we want names in signatures to point to the definitions
    collectType ty
    symtab <- asks inScope
    tell . catMaybes . for names $ \name ->
      LRef . Ref (hseSymOrIdentPos name) <$>
        M.lookup (Ctxed CTerm $ hseSymOrIdentValue name) symtab
    return M.empty

  FunBind _l matches ->
    M.unions <$> mapM parseMatch matches

  PatBind _l pat _typeTODO rhs _bindsTODO -> do
    collectRhs rhs
    collectPat pat

  -- TODO
  other -> tell [LWarn$ "xDECL " ++ show other] >> return M.empty

collectRhs :: Rhs S -> Parse ()
collectRhs rhs = case rhs of
  UnGuardedRhs _l exp -> collectExp exp
  GuardedRhss _l grhss ->
    mapM_ collectExp $ concat (map expsFrom grhss)
  where
  expsFrom (GuardedRhs _l stmts exp) = exp : (stmts >>= stmtExps)
  stmtExps stmt = case stmt of
    Qualifier _l exp -> [exp]
    other -> error $ "GuardedRhs statement " ++ show other

collectExp :: Exp S -> Parse ()
collectExp exp = case exp of
  Var _l qname -> parseQName CTerm qname
  Con _l qname -> parseQName CTerm qname
  InfixApp _l lexp _op rexp -> exps [lexp, rexp]
  App _l exp1 exp2 -> exps [exp1, exp2]
  NegApp _l exp -> collectExp exp
  Case _l exp alts -> do
    collectExp exp
    mapM_ collectAlt alts
  Do _l statements -> void $ parseStatementsNonRecursive statements
  If _l c a b -> exps [c, a, b]
  Let _l binds exp -> do
    binds_symtab <- parseBinds binds
    local (withLocalSymtab binds_symtab) $ collectExp exp
  Paren _l exp -> collectExp exp
  Tuple _l _boxed es -> exps es 
  List _l ee -> exps ee
  Lambda _l pats exp -> do
    pattern_symtab <- M.unions <$> mapM collectPat pats
    local (withLocalSymtab pattern_symtab) $ collectExp exp
  LeftSection _l exp qop -> parseSection qop exp
  RightSection _l qop exp -> parseSection qop exp
  other -> tell [LWarn$ "Exp " ++ show exp]
  where
  exps ee = mapM_ collectExp ee
  parseSection qop exp = parseQOp CTerm qop >> collectExp exp

collectAlt :: Alt S -> Parse ()
collectAlt (Alt _l pat guarded_alts binds) = do
  -- only used in guarded_alts
  pattern_symtab <- collectPat pat
  local (withLocalSymtab pattern_symtab) $ collectGuardedAlts guarded_alts

collectGuardedAlts :: GuardedAlts S -> Parse ()
collectGuardedAlts gas = case gas of
  UnGuardedAlt _l exp -> collectExp exp
  other -> tell [LWarn$ "Guarded case alternative: " ++ show other]

collectPat :: Pat S -> Parse SymTab
collectPat p = case p of
  PVar _l name -> return . singletonPair $ hseNameToSymbolAndLoc CTerm name
  PApp _l qname pats -> do
    parseQName CTerm qname
    M.unions <$> mapM collectPat pats
  PAsPat _l name pat -> do
    rest_symtab <- collectPat pat
    return . insertPair (hseNameToSymbolAndLoc CTerm name) $ rest_symtab
  PParen _l pat -> collectPat pat
  PList _l pats -> M.unions <$> mapM collectPat pats
  PInfixApp _l p1 qname p2 -> do
    parseQName CTerm qname
    M.union <$> collectPat p1 <*> collectPat p2
  PTuple _l _boxed pats ->
    M.unions <$> mapM collectPat pats
  other -> tell [LWarn$ "xPat" ++ show other] >> return M.empty

parseMatch :: Match S -> Parse SymTab
parseMatch m = do
  let (name, patterns, rhs, mb_binds) = case m of
        Match _l nm ps r bs -> (nm, ps, r, bs)
        InfixMatch _l p nm ps r bs -> (nm, p:ps, r, bs)
  -- pattern symbols are not returned to the global scope, but used
  -- in resolving RHS and Binds
  pattern_symtab <- M.unions <$> mapM collectPat patterns
  -- bind symbols are used only for resolving RHS
  binds_symtab <- case mb_binds of
    Nothing -> return M.empty
    Just binds -> local (withLocalSymtab pattern_symtab) $ parseBinds binds
  -- TODO is there precedence order between binds and patterns?
  local (withLocalSymtab (binds_symtab `M.union` pattern_symtab)) $
    collectRhs rhs
  return . singletonPair $ hseNameToSymbolAndLoc CTerm name

parseBinds :: Binds S -> Parse SymTab
parseBinds binds = case binds of
  BDecls _l decls -> do
    rec decl_symtab <- local (withLocalSymtab decl_symtab) $
                         M.unions <$> mapM collectDecl decls
    return decl_symtab
  IPBinds _l ipbinds -> error "no IPBind support"

parseStatementsRecursive :: [Stmt S] -> Parse SymTab
parseStatementsRecursive stmts = do
  rec actions_symtab <- local (withLocalSymtab actions_symtab) $
                          M.unions <$> mapM parseStatement stmts
  return actions_symtab

parseStatementsNonRecursive :: [Stmt S] -> Parse SymTab
parseStatementsNonRecursive stmts = do
  -- add new symbols linearly to the symtab
  symtab <- asks inScope
  foldM (\syms stmt -> (`M.union` syms) <$>
                          local (setSymtab syms) (parseStatement stmt))
        symtab stmts
 
parseStatement :: Stmt S -> Parse SymTab
parseStatement stmt = case stmt of
  Generator _l pat exp -> collectExp exp >> collectPat pat
  Qualifier _l exp -> collectExp exp >> return M.empty
  LetStmt _l binds -> parseBinds binds
  RecStmt _l stmts -> parseStatementsRecursive stmts

collectDeclHead :: DeclHead S -> SymbolAndLoc
collectDeclHead dhead = case dhead of
  DHead _hl name _tyvarbinds -> hseNameToSymbolAndLoc CType name
  DHInfix _hl _tyleft name _tyright -> hseNameToSymbolAndLoc CType name
  DHParen _hl innerHead -> collectDeclHead innerHead

-- | Parses a constructor declaration.
collectQualConDecl :: Symbol -> QualConDecl S -> Parse SymTab
collectQualConDecl tyname (QualConDecl _l _tyvarbinds ctx ctor_decl) =
  -- TODO ctx
  case ctor_decl of
    ConDecl _l name bangs -> ctorBangs name bangs
    InfixConDecl _l lbang name rbang -> ctorBangs name [lbang, rbang]
    RecDecl _l name field_decls -> do
      let ctor_symloc = hseNameToSymbolAndLoc CTerm name
      field_symlocs <- M.unions <$> mapM collectFieldDecl field_decls
      mapM_ (tell . mkChild' tyname) (M.keys field_symlocs)
      return $ insertPair ctor_symloc field_symlocs
  where
  ctorBangs name bangs = do
    tell . mkChild' tyname $ Ctxed CTerm (hseSymOrIdentValue name)
    mapM_ (collectType . bangType) bangs
    return . M.fromList $ [hseNameToSymbolAndLoc CTerm name]

collectFieldDecl :: FieldDecl S -> Parse SymTab
collectFieldDecl (FieldDecl l names bang) = do
  collectType . bangType $ bang
  return . M.fromList . map (hseNameToSymbolAndLoc CTerm) $ names

bangType :: BangType l -> Type l
bangType (BangedTy _ t) = t
bangType (UnBangedTy _ t) = t
bangType (UnpackedTy _ t) = t

collectType :: Type S -> Parse ()
collectType ty = case ty of
  TyCon _l qname -> parseQName CType qname
  TyFun _l t1 t2 -> mapM_ collectType [t1, t2]
  other -> tell [LWarn$ "xTyCon " ++ show ty]

parseQOp :: Ctx -> QOp S -> Parse ()
parseQOp ctx op = 
  let qname = case op of
        QVarOp _l q -> q
        QConOp _l q -> q  -- TODO what is the difference?
  in parseQName ctx qname

parseQName :: Ctx -> QName S -> Parse ()
parseQName ctx qname = do
  s <- asks inScope
  tell . maybeToList $ do  -- in Maybe
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

mkChild' p c = [LChild p c]

-- TODO remove
mkChild :: Symbol -> Symbol -> a -> Logs
mkChild p c = const [LChild p c]

insertPair (a,b) = M.insert a b
singletonPair p = insertPair p M.empty
  
applyIf :: Bool -> (a -> a) -> a -> a
applyIf c f = if c then f else id
