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
  { inScope :: SymTab
  , parseOpts :: ()
  } 

type ParseT m a = RWST ParseCtx Logs () m a
type Parse a = ParseT Identity a

-- * 

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

type SymK = Ctxed String
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

data Log = LRef Ref | LWarn String | LChild (Ctxed String) (Ctxed String)
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
      EModuleContents _ (ModuleName _ mname) -> error "EModuleContents"
    getQName ctx =
      -- TODO maybe should drop module part instead flatten? what's the
      --    spec here?
      maybeToList . fmap (Ctxed ctx . snd) . flattenQName

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
    n : M.findWithDefault [] n children
  IThingWith _ name cnames ->
    getName CType name ++ map (Ctxed CTerm . getCName) cnames
  where
    getName :: Ctx -> Name S -> [SymK]
    getName ctx = return . fst . mkNameSym ctx

importSyms :: Map MName ModuleInfo -> [ImportDecl S] -> SymTab
importSyms modules = mconcat . map getImports
  where
    getImports (ImportDecl _l lmname isQual _src _pkg alias specs) =
      fromMaybe M.empty $ for (M.lookup (mname lmname) modules) (\mi ->
      let exports = miExports mi
          filtered = maybe id (filterImports$ miChildren mi) specs exports
          ifNotQualFilteredElse r = cond isQual r filtered
          aliased mAlias =
            let prefix = mAlias ++ "."
            in M.mapKeys (fmap (prefix ++)) filtered
      in case alias of
           Just a -> aliased (mname a) `mappend`
                       ifNotQualFilteredElse M.empty
           Nothing -> ifNotQualFilteredElse . aliased $ mname lmname
      )
    mname (ModuleName _ m) = m
    filterImports :: ChildMap -> ImportSpecList S -> SymTab -> SymTab
    filterImports children (ImportSpecList _ isHiding iss) syms =
      let selected = iss >>= imports children -- TODO use set
          passes s = invertIf isHiding $ s `elem` selected
      in M.filterWithKey (\s _ -> passes s) syms

collectModule :: Map MName ModuleInfo -> Module S -> ModuleInfo
collectModule modules (Module _l mhead _pragmas imports decls) =
  let (declSyms, _, declLogs) = runRWS (mapM collectDecl decls) pctx ()
      mname = moduleName <$> mhead
      importSymTab = importSyms modules imports
      moduleSymTab = M.map (\s -> s { symModule = mname }) $ M.unions declSyms
      symTab = moduleSymTab `M.union` importSymTab
      pctx = ParseCtx { inScope = symTab, parseOpts = () }
      logs = declLogs ++ [LWarn . ("IMPORTS " ++ ) . show $ importSymTab]
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
    exportedChildren :: SymTab -> ChildMap -> ChildMap
    exportedChildren exports =
      let exported = flip M.member exports
      in M.map (filter exported) . M.filterWithKey (\s _ -> exported s)

mergeCollect :: [SymTab -> Logs] -> SymTab -> Logs
mergeCollect fs s = concatMap ($ s) fs

collectDecl :: Decl S -> Parse SymTab
collectDecl decl = do
  syms <- asks inScope
  let (topSymKvs, fun) = collectDecl' decl
      subLogs = fun syms
  tell subLogs
  return . M.fromList $ topSymKvs

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
          LRef . Ref (namePos name) <$> M.lookup (Ctxed CTerm$ nameVal name) s
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
  other -> const [LWarn$ "GuardedRhs " ++ show other]

collectExp :: Exp S -> SymTab -> Logs
collectExp exp = case exp of
  Var _l qname -> resolveQName CTerm qname
  Con _l qname -> resolveQName CTerm qname
  InfixApp _l lexp _op rexp -> exps [lexp, rexp]
  App _l exp1 exp2 -> exps [exp1, exp2]
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

mkNameSym :: Ctx -> Name S -> SymKV
mkNameSym ctx name = (Ctxed ctx (nameVal name), namePos name)

mkChild :: Ctxed String -> Ctxed String -> a -> Logs
mkChild p c = const [LChild p c]

collectQualConDecl :: Ctxed String -> QualConDecl S -> Collector
collectQualConDecl tyname (QualConDecl _l _tyvarbinds ctx conDecl) =
  -- TODO ctx
  case conDecl of
    ConDecl _l name bangs -> ctorBangs name bangs
    InfixConDecl _l lbang name rbang -> ctorBangs name [lbang, rbang]
    RecDecl _l name fields ->
      let ctorSym = mkNameSym CTerm name
          (fieldSyms, fs) = unzip $ map collectFieldDecl fields
          flatSyms = concat fieldSyms
          tyChildren = for flatSyms (mkChild tyname . fst)
      in (ctorSym:flatSyms, mergeCollect (fs ++ tyChildren))
  where
    bangCollect = mergeCollect . map (collectType . bangType)
    ctorBangs name bangs =
      ([mkNameSym CTerm name],
        mergeCollect [mkChild tyname (Ctxed CTerm$ nameVal name),
                      bangCollect bangs])

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

resolveQName :: Ctx -> QName S -> SymTab -> Logs
resolveQName ctx qname s = maybeToList $ do
  (l, name) <- flattenQName qname
  refPos <- M.lookup (Ctxed ctx name) s
  return . LRef $ Ref (wrapLoc l) refPos

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
