{-# LANGUAGE RecursiveDo, FlexibleContexts, FlexibleInstances, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-- TODO remove this once HSE 0.15.0.2 is out
-- See https://github.com/haskell-suite/haskell-src-exts/issues/42
{-# LANGUAGE Arrows #-} 
module Language.Haskell.Nemnem.Parser where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad.Identity
import Control.Monad.Trans.RWS
import Data.Aeson.Types as A
import qualified Data.DList as DList
import Data.Either
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Language.Haskell.Exts.Annotated as HSE

import Language.Haskell.Nemnem.Util

type LhsSymbols = [SymbolAndLoc]

data ParseCtx = ParseCtx
  { inScope :: SymTab  -- ^ Tied recursively.
  -- | TODO make a bit more versatile, able to represent contexts such as
  -- comments, RULES pragmas, imports/exports..
  , definitionStack :: [LhsSymbols]
  , parseOpts :: ()
  }

showDefinitionStack =
  T.intercalate "/" . reverse . map (
    T.intercalate "," . map (T.pack . dropCtx . fst) ) 

withLocalSymtab :: SymTab -> ParseCtx -> ParseCtx
withLocalSymtab symtab pctx =
  -- M.union is left-biased, which is what we want here.
  pctx { inScope = symtab `M.union` inScope pctx }

pushDefs :: LhsSymbols -> ParseCtx -> ParseCtx
pushDefs lhss pctx =
  pctx { definitionStack = lhss:definitionStack pctx }

pushDef a = pushDefs [a] 

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

-- TODO add Hint (such as RecordField, ClassField..) to Sym(Loc) to aid
--      semantic highlighting. Alternative: use child relations?
type Symbol = Ctxed String
data SymLoc = SymLoc
  { symRange :: LineColRange
  -- | When parsing the module AST, initially filled with Nothing,
  -- which is substituted later by parseModuleSymbols / collectModule
  -- before tying back the knot.
  , symModule :: Maybe String  -- TODO these Strings are ugly
  }
  deriving (Eq, Show)

-- TODO rather transform these structures in the serving code and add
-- instances there? 
instance ToJSON SymLoc where
  toJSON (SymLoc sym_range sym_module) = object
    [ "range" .= sym_range
    , "module" .= sym_module ]

type SymbolAndLoc = (Symbol, SymLoc)
type SymTab = Map Symbol SymLoc

-- Ref is now suitable for generating visual hyperrefs, but not much for
-- searching / indexing / more intelligent displays. 
-- TODO should rather be SymbolAndLoc? 
data Ref = Ref
  { refSource :: SymLoc
  , refTarget :: SymLoc
  , refDefinitionStack :: [LhsSymbols]
  }
  deriving Show

instance ToJSON Ref where
  toJSON (Ref s t stack) = object
    [ "source" .= s
    , "target" .= t
    , "stack" .= showDefinitionStack stack ]

lineCol :: S -> LineColRange
lineCol src = 
  let s = srcInfoSpan src
  in ((srcSpanStartLine s, srcSpanStartColumn s),
      (srcSpanEndLine s, srcSpanEndColumn s))

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

-- * Link collector part

data Warn = Warn LineColRange String
  deriving (Show)

data Highlight = Highlight LineColRange HighlightKind
  deriving (Show)

data VariableDeclKind 
  = MatchHead
  | RecordField
  | OtherVarDecl
  deriving (Show)

-- TODO move highlights to module, use qualified
data HighlightKind
  = Literal LiteralKind
  -- TODO add Depth Int based on stack depth to some of these
  | VariableDecl VariableDeclKind
  | SpecialName  -- used for as-pattern head now
  | TypeVariable
  | TypeConstructorDecl
  | TypeConstructorRef
  | CommentHL
  | Pragma
  -- | Infix application of functions, operators.
  | Infix
  | VariableRef
  | ConstructorDecl
  | ConstructorRef
  | ClassDeclHL
  | ClassRef
  {-
  | Constructor
  | TypeLevel
  | TermLevel
  | OtherKeyword
  -}
  deriving (Show)

data LiteralKind = CharLit | StringLit | NumLit
  deriving (Show)

getCommentHighlight (Comment _ l _) =
  Highlight (lineCol . noInfoSpan $ l) CommentHL

-- TODO add module references, where imported module name points to module
--      source
data Log
  = LRef Ref
  | LWarn Warn
  | LChild Symbol Symbol
  | LHighlight Highlight
  deriving Show

getRef (LRef r) = [r]
getRef _ = []
getWarn (LWarn w) = [w]
getWarn _ = []
getChild (LChild p c) = [(p, c)]
getChild _ = []
getHighlight (LHighlight h) = [h]
getHighlight _ = []

warnMay mb_elem w = case mb_elem of
  Nothing -> return ()
  Just elem -> warn elem w
warn elem = tell . DList.singleton . warnAt elem
warnAt annotated_elem warning = LWarn $
  Warn (lineCol . ann $ annotated_elem) (warning ++ take 30 (show annotated_elem))

highlight l = tell . DList.singleton . highlightAt l
highlightAt l = highlightAt' (lineCol l)
highlightAt' range hl_kind = LHighlight $ Highlight range hl_kind

type Logs = DList.DList Log

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
  , miWarns :: [Warn]
  , miHighlights :: [Highlight]
  , miOriginalPath :: Maybe String
  }

--      EModuleContents _ (ModuleName _ mname) ->

exportedKeys :: Map MName ModuleInfo -> ChildMap -> [ExportSpec l]
             -> ([Symbol], [ModuleInfo], SymTab)
exportedKeys modules children xs =
  let direct_exports = xs >>= directExports
      reexport_infos = catMaybes . map reexportedModuleInfo $ xs
      reexport_symtab = M.unions . map miExports $ reexport_infos
  in (direct_exports, reexport_infos, reexport_symtab)
  where
    reexportedModuleInfo x = case x of
      EModuleContents _ (ModuleName _ mname) -> M.lookup mname modules
      _ -> Nothing
    directExports x = case x of
      EVar _ qname -> getQName CTerm qname
      EAbs _ qname -> getQName CType qname
      EThingAll _ qname -> do
        name <- getQName CType qname
        name : M.findWithDefault [] name children
      EThingWith _ qname cnames ->
        getQName CType qname ++ map (Ctxed CTerm . getCName) cnames
      EModuleContents _ _ ->
        -- Handled elsewhere
        []
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
  logImport import_syms sym_and_loc@(symbol, loc) =
    case M.lookup symbol import_syms of
      Nothing -> return ()
      -- TODO represent the module-import correctly, not mixed with stack
      Just remote_loc ->
        tell . DList.singleton . LRef $ Ref loc remote_loc [[sym_and_loc]]

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
  let (symtab, _, dlogs) = runRWS (parseModuleSymbols modules m) pctx ()
      logs = DList.toList dlogs
      pctx = ParseCtx
              { inScope = symtab
              , definitionStack = []
              , parseOpts = () 
              }
      children =
        let pairs = map (\(p,c) -> (p, [c])) $ logs >>= getChild
        in M.unionsWith (++) (map (M.fromList . return) pairs)
      (exports, reexport_children) = fromMaybe (M.empty, M.empty) $
                                       headExports symtab children <$> mhead
      module_name = moduleHeadName <$> mhead
  in ModuleInfo
      { miName = module_name
      , miSymbols = symtab
      , miExports = exports
      -- TODO also include children from reexports
      , miChildren = exportedChildren exports children `M.union` reexport_children
      , miRefs = map (fillModuleName module_name)$ logs >>= getRef
      , miWarns = logs >>= getWarn
      , miHighlights = logs >>= getHighlight
      , miOriginalPath = Nothing
      }
  where
  headExports symtab children (ModuleHead _ _ _ exportSpecs) =
    case exportSpecs of
      -- TODO this is incorrect, since `symtab` at this point already includes
      --      the module-defined and imported symbols. Treat those separately,
      --      at least up to this point.
      Nothing -> (symtab, M.empty)
      Just (ExportSpecList _ xs) ->
        let (key_set, reexport_infos, reexport_symtab) =
              exportedKeys modules children xs
        -- TODO improve lookup efficiency
            result_symtab = M.filterWithKey (\k _ -> k `elem` key_set) symtab
                              `M.union` reexport_symtab
            reexport_children = M.unions . map miChildren $ reexport_infos
        in (result_symtab, reexport_children)
  exportedChildren :: SymTab -> ChildMap -> ChildMap
  exportedChildren exports =
    let exported = flip M.member exports
    in M.map (filter exported) . M.filterWithKey (\s _ -> exported s)
  fillModuleName module_name (Ref s t stack) =
    Ref (fill s) (fill t) stack
    where
    fill loc@(SymLoc _ Nothing) = loc { symModule = module_name }
    fill loc = loc

collectDecl :: Decl S -> Parse SymTab
collectDecl = collectDeclWithSigExports False

collectDeclWithSigExports :: Bool -> Decl S -> Parse SymTab
collectDeclWithSigExports signature_export decl = case decl of
  DataDecl _l _dataOrNew mb_ctx decl_head quals mb_derivings -> do
    warnMay mb_ctx "DataDecl Context"
    warnMay mb_derivings "DataDecl Derivings"
    head_symloc@(head_sym, _) <- collectDeclHead TypeConstructorDecl decl_head
    ctor_symtab <- M.unions <$> mapM (collectQualConDecl head_sym) quals
    return $ insertPair head_symloc ctor_symtab

  ClassDecl _l mb_ctx decl_head fundeps mb_classdecls -> do
    warnMay mb_ctx "ClassDecl Context"
    mapM_ (flip warn "Fundep") fundeps
    head_symloc@(head_sym, _) <- collectDeclHead ClassDeclHL decl_head
    -- TODO should use recursive tying to resolve default methods, mutually
    --      recursive types
    decl_symtab <- maybe (return M.empty)
                         (\decls -> M.unions <$>
                                      mapM (parseClassDecl head_sym) decls)
                         mb_classdecls
    return $ insertPair head_symloc decl_symtab

  InstDecl _l mb_ctx inst_head mb_decls -> do
    warnMay mb_ctx "InstDecl Context"
    parseInstHead inst_head
    -- TODO if we really wanted, could feed back the symbols so
    --      overrides point to the current instance instead of 
    --      class methods (once classes are parsed).
    maybe (return ()) (mapM_ parseInstanceDecl) mb_decls
    return M.empty

  TypeSig _l names ty -> do
    collectType ty
    if signature_export then
        return . M.fromList . map (hseNameToSymbolAndLoc CTerm) $ names
      else do
        -- we want names in signatures to point to the definitions
        symtab <- asks inScope
        stack <- asks definitionStack
        tell . DList.fromList . catMaybes . for names $ \name ->
          LRef . (\remote_loc -> Ref (hseSymOrIdentPos name) remote_loc stack) <$>
            M.lookup (Ctxed CTerm $ hseSymOrIdentValue name) symtab
        return M.empty

  FunBind _l matches ->
    -- TODO don't export, and point to sig if signature_export == True
    if null matches then return M.empty
    else let (m:ms) = matches
         in do
              first_sym_and_loc <- parseMatch Nothing m
              rest <- mapM (parseMatch (Just . snd $ first_sym_and_loc)) ms
              -- Left-biased `M.unions` keeps first match as symbol.
              return . M.unions . map (uncurry M.singleton) $
                first_sym_and_loc:rest

  PatBind _l pat mb_type rhs mb_binds -> do
    -- TODO don't export, and point to sig if signature_export == True
    warnMay mb_binds "PatBind binds"
    warnMay mb_type "PatBind type"
    defined_sym_and_locs <- collectPat pat
    local (pushDefs . M.toList $ defined_sym_and_locs) $ collectRhs rhs
    return defined_sym_and_locs

  InlineSig l _what_is_this_bool_TODO _activation qname -> do
    parseQName CTerm qname
    highlight l Pragma
    return M.empty

  RulePragmaDecl l _rules -> highlight l Pragma >> return M.empty

  other -> warn other "Decl" >> return M.empty

-- TODO could return TypeVars to local context (first they need to be
--      collected in collectType).
parseInstHead :: InstHead S -> Parse ()
parseInstHead ihead = case ihead of
  IHead _l qname tys -> do
    -- TODO introduce CClass ctx if can conflict with CType
    parseQName CType qname
    highlight (ann qname) ClassRef
    mapM_ collectType tys
  i@(IHInfix l t1 qname t2) -> warn i "IHInfix"
  IHParen _l ih -> parseInstHead ih

parseInstanceDecl :: InstDecl S -> Parse ()
parseInstanceDecl idecl = case idecl of
  InsDecl _l decl -> void $ collectDecl decl
  other -> warn other "InstDecl"

collectRhs :: Rhs S -> Parse ()
collectRhs rhs = case rhs of
  UnGuardedRhs _l exp -> collectExp exp
  GuardedRhss _l grhss ->
    mapM_ collectExp $ concat (map expsFrom grhss)
  where
  expsFrom (GuardedRhs _l stmts exp) = exp : (stmts >>= stmtExps)
  stmtExps stmt = case stmt of
    Qualifier _l exp -> [exp]
    other -> error "GuardedRhs"  -- warn other "GuardedRhs"]

collectExp :: Exp S -> Parse ()
collectExp exp = case exp of
  Var l qname -> parseQName CTerm qname >> highlight l VariableRef
  Con l qname -> parseQName CTerm qname >> highlight l ConstructorRef
  InfixApp _l lexp qop rexp -> do
    exps [lexp, rexp]
    highlight (ann qop) Infix
    parseQOp CTerm qop
  -- Note: might need to record application depth for semantic highlight
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
    -- TODO would it useful to make lambda part of the definitionStack?
    pattern_symtab <- M.unions <$> mapM collectPat pats
    local (withLocalSymtab pattern_symtab) $ collectExp exp
  LeftSection _l exp qop -> parseSection qop exp
  RightSection _l qop exp -> parseSection qop exp
  Lit l literal -> case literal of
    Char _ _ _ -> highlight l $ Literal CharLit
    HSE.String _ _ _ -> highlight l $ Literal StringLit
    Int _ _ _ -> highlight l $ Literal NumLit
    Frac _ _ _ -> highlight l $ Literal NumLit
    _ -> return ()
  ExpTypeSig _l exp ty -> do
    collectExp exp
    collectType ty
  other -> warn other "Exp"
  where
  exps ee = mapM_ collectExp ee
  parseSection qop exp = do
    -- TODO repetitive parseQOp + highlight
    collectExp exp
    highlight (ann qop) Infix
    parseQOp CTerm qop

collectAlt :: Alt S -> Parse ()
collectAlt (Alt _l pat guarded_alts mb_binds) = do
  -- only used in locally
  pattern_symtab <- collectPat pat
  binds_symtab <- parseMaybeBinds pattern_symtab mb_binds
  local (withLocalSymtab $ pattern_symtab `M.union` binds_symtab) $
    collectGuardedAlts guarded_alts

collectGuardedAlts :: GuardedAlts S -> Parse ()
collectGuardedAlts gas = case gas of
  UnGuardedAlt _l exp -> collectExp exp
  GuardedAlts _l galts -> mapM_ parseGuardedAlt galts

parseGuardedAlt :: GuardedAlt S -> Parse ()
parseGuardedAlt (GuardedAlt _l stmts exp) = do
  defined_symtab <- parseStatementsNonRecursive stmts
  local (withLocalSymtab defined_symtab) $ collectExp exp

collectPat :: Pat S -> Parse SymTab
collectPat p = case p of
  PVar _l name -> do
    highlight (ann name) $ VariableDecl OtherVarDecl
    return . singletonPair $ hseNameToSymbolAndLoc CTerm name
  PApp _l qname pats -> do
    -- TODO helper for parseQName + highlight
    parseQName CTerm qname >> highlight (ann qname) ConstructorRef
    M.unions <$> mapM collectPat pats
  PWildCard _l -> return M.empty
  PAsPat _l name pat -> do
    highlight (ann name) SpecialName
    rest_symtab <- collectPat pat
    return . insertPair (hseNameToSymbolAndLoc CTerm name) $ rest_symtab
  PParen _l pat -> collectPat pat
  PList _l pats -> M.unions <$> mapM collectPat pats
  PInfixApp _l p1 qname p2 -> do
    parseQName CTerm qname
    highlight (ann qname) Infix
    M.union <$> collectPat p1 <*> collectPat p2
  PTuple _l _boxed pats ->
    M.unions <$> mapM collectPat pats
  PBangPat _l pat -> collectPat pat
  other -> warn other "Pat" >> return M.empty

-- | Parses the subtree by pushing the Match under definition to the
-- `definitionStack`.
parseMatch :: Maybe SymLoc -> Match S -> Parse SymbolAndLoc
parseMatch mb_first_match_loc m = do
  let (name, patterns, rhs, mb_binds) = case m of
        Match _l nm ps r bs -> (nm, ps, r, bs)
        InfixMatch _l p nm ps r bs -> (nm, p:ps, r, bs)
      defined_sym_and_loc = hseNameToSymbolAndLoc CTerm name
  highlight (ann name) $ VariableDecl MatchHead
  case mb_first_match_loc of
    Nothing -> return ()
    Just first_match_loc -> do
      -- Emit reference to first match
      -- TODO this is a pseudo-reference, annotate Ref (also for pseudo-ref
      --      coming from typesig)
      stack <- asks definitionStack
      tell . DList.singleton . LRef $
        Ref (snd defined_sym_and_loc) first_match_loc stack
  local (pushDef defined_sym_and_loc) $ do
    -- pattern symbols are not returned to the global scope, but used
    -- in resolving RHS and Binds
    pattern_symtab <- M.unions <$> mapM collectPat patterns
    -- bind symbols are used only for resolving RHS
    binds_symtab <- parseMaybeBinds pattern_symtab mb_binds
    -- TODO is there precedence order between binds and patterns?
    local (withLocalSymtab (binds_symtab `M.union` pattern_symtab)) $
      collectRhs rhs
    return defined_sym_and_loc

parseMaybeBinds :: SymTab -> Maybe (Binds S) -> Parse SymTab
parseMaybeBinds extra_symtab mb_binds = case mb_binds of
  Nothing -> return M.empty
  Just binds -> local (withLocalSymtab extra_symtab) $ parseBinds binds

parseBinds :: Binds S -> Parse SymTab
parseBinds binds = case binds of
  BDecls _l decls -> do
    rec decl_symtab <- local (withLocalSymtab decl_symtab) $
                         M.unions <$> mapM collectDecl decls
    return decl_symtab
  other -> warn other "Binds" >> return M.empty

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

collectDeclHead :: HighlightKind -> DeclHead S -> Parse SymbolAndLoc
collectDeclHead hl_kind dhead = case dhead of
  DHead _l name tyvarbinds -> collectHeadAndBinds name tyvarbinds
  DHInfix _l ty1 name ty2 -> collectHeadAndBinds name [ty1, ty2]
  DHParen _l innerHead -> collectDeclHead hl_kind innerHead
  where
  collectHeadAndBinds hd binds = do
    highlight (ann hd) hl_kind
    mapM_ (\b -> highlight (ann b) TypeVariable) binds
    return $ hseNameToSymbolAndLoc CType hd

parseClassDecl :: Symbol -> ClassDecl S -> Parse SymTab
parseClassDecl clsname cdecl = case cdecl of
  ClsDecl _l decl -> do
    decl_symtab <- collectDeclWithSigExports True decl
    -- TODO repetitive
    mapM_ (tell . DList.fromList . mkChild' clsname) (M.keys decl_symtab)
    return decl_symtab
  other -> warn other ("ClassDecl " ++ take 30 (show other)) >> return M.empty

-- | Parses a constructor declaration.
collectQualConDecl :: Symbol -> QualConDecl S -> Parse SymTab
collectQualConDecl tyname (QualConDecl _l mb_tyvarbinds mb_ctx ctor_decl) =
  -- TODO mb_ctx, mb_tyvarbinds
  case ctor_decl of
    ConDecl _l name bangs -> ctorBangs name bangs
    InfixConDecl _l lbang name rbang -> ctorBangs name [lbang, rbang]
    RecDecl _l name field_decls -> do
      let ctor_symloc = hseNameToSymbolAndLoc CTerm name
      highlightName name
      field_symlocs <- M.unions <$> mapM collectFieldDecl field_decls
      mapM_ (tell . DList.fromList . mkChild' tyname) (M.keys field_symlocs)
      return $ insertPair ctor_symloc field_symlocs
  where
  highlightName name = highlight (ann name) ConstructorDecl
  ctorBangs name bangs = do
    highlightName name
    tell . DList.fromList . mkChild' tyname $
      Ctxed CTerm (hseSymOrIdentValue name)
    mapM_ (collectType . bangType) bangs
    return . M.fromList $ [hseNameToSymbolAndLoc CTerm name]

collectFieldDecl :: FieldDecl S -> Parse SymTab
collectFieldDecl (FieldDecl l names bang) = do
  collectType . bangType $ bang
  mapM_ (\n -> highlight (ann n) (VariableDecl RecordField)) names
  return . M.fromList . map (hseNameToSymbolAndLoc CTerm) $ names

bangType :: BangType l -> Type l
bangType (BangedTy _ t) = t
bangType (UnBangedTy _ t) = t
bangType (UnpackedTy _ t) = t

collectType :: Type S -> Parse ()
collectType ty = case ty of
  TyCon l qname -> parseQName CType qname >> highlight l TypeConstructorRef
  TyFun _l t1 t2 -> mapM_ collectType [t1, t2]
  -- Note: for semantic highlight we might need to keep track
  -- the depth of the application?
  TyApp _l t1 t2 -> mapM_ collectType [t1, t2]
  TyParen _l t -> collectType t
  -- TODO create implicit entity for variable, link them together
  TyVar l _name -> highlight l $ TypeVariable
  TyTuple _l _boxed tys -> mapM_ collectType tys
  TyList _l t -> collectType t
  other -> warn other "Type"

parseQOp :: Ctx -> QOp S -> Parse ()
parseQOp ctx op = 
  let qname = case op of
        QVarOp _l q -> q
        QConOp _l q -> q  -- TODO what is the difference?
  in parseQName ctx qname

parseQName :: Ctx -> QName S -> Parse ()
parseQName ctx qname = do
  s <- asks inScope
  stack <- asks definitionStack
  tell . DList.fromList . maybeToList $ do  -- in Maybe
        (l, name) <- flattenQName qname
        refPos <- M.lookup (Ctxed ctx name) s
        return . LRef $ Ref (wrapLoc l) refPos stack

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

insertPair (a,b) = M.insert a b
singletonPair p = insertPair p M.empty
  
applyIf :: Bool -> (a -> a) -> a -> a
applyIf c f = if c then f else id
