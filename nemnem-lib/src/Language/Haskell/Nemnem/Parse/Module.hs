{-# LANGUAGE RecursiveDo, FlexibleContexts, FlexibleInstances, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-- TODO remove this once HSE 0.15.0.2 is out
-- See https://github.com/haskell-suite/haskell-src-exts/issues/42
{-# LANGUAGE Arrows #-} 
module Language.Haskell.Nemnem.Parse.Module where

import Control.Applicative ((<$>), (<*>), (<|>), pure)
import Data.Functor.Identity
import Control.Monad (foldM, void, when)
import Control.Monad.Trans.RWS
import Data.Aeson.Types as A
import qualified Data.DList as DList
import qualified Data.List as L
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Language.Haskell.Exts.Annotated as HSE

import Language.Haskell.Nemnem.Internal.Util

type LhsSymbols = [SymbolAndLoc]

data ParseCtx = ParseCtx
  { inScope :: SymTab  -- ^ Tied recursively.
  -- | TODO make a bit more versatile, able to represent contexts such as
  -- comments, RULES pragmas, imports/exports, class/instance decls, package,
  -- etc..
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

pushDef :: SymbolAndLoc -> ParseCtx -> ParseCtx
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
  -- TODO symPackage
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

makeCommentHighlight (Comment _ l _) =
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
  Warn (lineCol . ann $ annotated_elem) (warning ++ take 530 (show annotated_elem))

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
        -- Handled above in reexportedModuleInfo
        []
    getQName ctx =
      -- Conversion to unqualified name happens at callsite.
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
      Nothing -> do
        -- Imported module not yet processed?
        warn lmname "Module not found"
        return mempty  
      Just mi -> do
        filtered <-
          let exports = miExports mi
          in case mb_specs of
            Just specs -> filterImports (miChildren mi) specs exports
            Nothing -> return exports
        let aliased_symtab = 
              let alias_name = mname . fromMaybe lmname $ alias
              in aliased alias_name filtered
        return $ aliased_symtab `mappend` if isQual then M.empty else filtered
  --
  aliased mAlias exps =
    let prefix = mAlias ++ "."
       -- TODO this won't work for operator-like funny chars,
       --      what is the rule there?
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

data AvailableSymbols = ASymbols
  { aSymImported :: SymTab
  , aSymDefined :: SymTab
  }

mkAvailableSymbols = ASymbols

allSymbols :: AvailableSymbols -> SymTab
allSymbols (ASymbols imported defined) = defined `M.union` imported

parseModuleSymbols :: Map MName ModuleInfo -> Module S -> Parse AvailableSymbols
parseModuleSymbols modules (Module _l mhead _pragmas imports decls) = do
  decl_symtabs <- mapM collectDecl decls
  let mname = moduleHeadName <$> mhead
      module_symtab = M.map (\s -> s { symModule = mname }) $
                        M.unions decl_symtabs
  imported_symtab <- importSyms modules imports
  return $ mkAvailableSymbols imported_symtab module_symtab

collectModule :: Map MName ModuleInfo -> Module S -> ModuleInfo
collectModule modules m@(Module _l mhead _pragmas imports decls) =
  let (av_symtab, _, dlogs) = runRWS (parseModuleSymbols modules m) pctx ()
      logs = DList.toList dlogs
      pctx = ParseCtx
              { inScope = allSymbols av_symtab
              , definitionStack = []
              , parseOpts = () 
              }
      children =
        let pairs = map (\(p,c) -> (p, [c])) $ logs >>= getChild
        in M.unionsWith (++) (map (M.fromList . return) pairs)
      (exports, reexport_children) = fromMaybe (M.empty, M.empty) $
                                       headExports av_symtab children <$> mhead
      module_name = moduleHeadName <$> mhead
  in {-# SCC collectModule #-} ModuleInfo
      { miName = module_name
      , miSymbols = aSymDefined av_symtab
      -- TODO add imported symbols to ModuleInfo if needed
      , miExports = exports
      , miChildren = exportedChildren exports children `M.union` reexport_children
      , miRefs = map (fillModuleName module_name)$ logs >>= getRef
      , miWarns = logs >>= getWarn
      , miHighlights = logs >>= getHighlight
      , miOriginalPath = Nothing
      }
  where
  headExports av_symtab children (ModuleHead _ _ _ exportSpecs) =
    case exportSpecs of
      Nothing -> (aSymDefined av_symtab, M.empty)
      -- TODO handle self-reexport (module Test (module Test, ..) where ..)
      Just (ExportSpecList _ xs) ->
        let (key_set, reexport_infos, reexport_symtab) =
              exportedKeys modules children xs
            result_symtab = 
              let export_symtab =
                    let symtab = allSymbols av_symtab
                        keyInKeySet k _ = k `elem` key_set
                        toUnqualified (Ctxed ctx a) =
                          -- TODO operator-like funny chars
                          Ctxed ctx . reverse . fst . L.break (== '.') . reverse $ a
                    in M.mapKeys toUnqualified . M.filterWithKey keyInKeySet $
                         symtab
              in export_symtab `M.union` reexport_symtab
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
collectDecl = collectDeclWithSigExports False False

-- | Arguments:
-- `signature_export`: if true, we associate the signature declaration
-- of a function with the related symbol, so that usages point to the signature.
-- Expected to be set only inside class declarations.
--
-- `signature_link`: if true, function declarations should link to the signature
-- of that function. Expected to be set inside instance declarations.
collectDeclWithSigExports :: Bool -> Bool -> Decl S -> Parse SymTab
collectDeclWithSigExports signature_export signature_link decl = case decl of
  DataDecl _l _dataOrNew mb_ctx decl_head quals mb_derivings -> do
    warnMay mb_ctx "DataDecl Context"
    warnMay mb_derivings "DataDecl Derivings"
    head_symloc@(head_sym, _) <- collectDeclHead TypeConstructorDecl decl_head
    ctor_symtab <- M.unions <$> mapM (collectQualConDecl head_sym) quals
    return $ insertPair head_symloc ctor_symtab

  -- Note: take caution when adding symbols to local symtab in class/instance
  -- decl, to avoid resolving a link to the local decl instead of the instance
  -- for an other type.

  ClassDecl _l mb_ctx decl_head fundeps mb_classdecls -> do
    warnMay mb_ctx "ClassDecl Context"
    mapM_ (flip warn "Fundep") fundeps
    -- TODO pushDef once supported
    head_symloc@(head_sym, _) <- collectDeclHead ClassDeclHL decl_head
    decl_symtab <- maybe (return M.empty)
                         (\decls -> M.unions <$>
                                      mapM (parseClassDecl head_sym) decls)
                         mb_classdecls
    return $ insertPair head_symloc decl_symtab

  InstDecl _l mb_ctx inst_head mb_decls -> do
    warnMay mb_ctx "InstDecl Context"
    -- TODO pushDef once supported
    parseInstHead inst_head
    maybe (return ()) (mapM_ parseInstanceDecl) mb_decls
    return M.empty

  TypeSig _l names ty -> do
    collectType ty
    if signature_export then
        return . M.fromList . map (hseNameToSymbolAndLoc CTerm) $ names
      else do
        -- we want names in signatures to point to the definitions
        addScopeReferences (hseNameToSymbolAndLoc CTerm) names
        return M.empty

  FunBind _l matches ->
    if null matches then return M.empty
    else do
      let (m:ms) = matches
      sig_ref <- if signature_link then do
                   symtab <- asks inScope
                   return $ M.lookup (Ctxed CTerm $ matchName m) symtab
                 else return Nothing
      first_sym_and_loc <- parseMatch sig_ref m
      let rest_ref = sig_ref <|> (Just . snd) first_sym_and_loc
      mapM_ (parseMatch rest_ref) ms
      return . exceptInSignatureExport $ singletonPair first_sym_and_loc

  PatBind _l pat mb_type rhs mb_binds -> do
    warnMay mb_type "PatBind type"
    pattern_symtab <- collectPat pat
    when signature_link . addScopeReferences id . M.toList $ pattern_symtab
    let local_pattern_symtab = if signature_link
          then M.empty
          else pattern_symtab
    -- Note: need to see examples for what can end up used in binds from the
    --       pattern.
    binds_symtab <- parseMaybeBinds local_pattern_symtab mb_binds
    let local_symtab = local_pattern_symtab `M.union` binds_symtab
    local (pushDefs . M.toList $ pattern_symtab) $
      local (withLocalSymtab local_symtab) $
        collectRhs rhs
    return $ exceptInSignatureExport pattern_symtab

  InlineSig l _what_is_this_bool_TODO _activation qname -> do
    parseQName CTerm qname
    highlight l Pragma
    return M.empty

  RulePragmaDecl l _rules -> highlight l Pragma >> return M.empty

  other -> warn other "Decl" >> return M.empty
  where
  -- | Use to prevent the default impl getting into symtab.
  exceptInSignatureExport :: SymTab -> SymTab
  exceptInSignatureExport m = if signature_export then M.empty else m

  addScopeReferences :: (a -> SymbolAndLoc) -> [a] -> Parse ()
  addScopeReferences symLocFun as = do
    symtab <- asks inScope
    stack <- asks definitionStack
    tell . DList.fromList . catMaybes . for as $ \a ->
      let (sym, loc) = symLocFun a
      in LRef . (\remote_loc -> Ref loc remote_loc stack) <$>
           M.lookup sym symtab

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

-- TODO return local SymTab and call with feedback, so local signature
--      definition can point to local function instead of class signature.
--      But this needs some thought, since adding it to local symtab is not
--      correct (since the class method can be called for other types).
--      Generally this might be impossible without typeclass inference.
parseInstanceDecl :: InstDecl S -> Parse ()
parseInstanceDecl idecl = case idecl of
  InsDecl _l decl -> void $ collectDeclWithSigExports False True decl
  other -> warn other "InstDecl"

collectRhs :: Rhs S -> Parse ()
collectRhs rhs = case rhs of
  UnGuardedRhs _l exp -> collectExp exp
  GuardedRhss _l grhss -> mapM_ expsFrom grhss
  where
  expsFrom (GuardedRhs _l stmts exp) = do
    mapM_ parseStatement stmts
    collectExp exp

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
  ListComp _l exp qual_stmts -> do
    stmts <- onlyStmts qual_stmts  -- no TransformListComp support (yet?)
    local_symtab <- parseStatementsNonRecursive stmts
    local (withLocalSymtab local_symtab) $ collectExp exp
  Lambda _l pats exp -> do
    -- TODO would it useful to make lambda part of the definitionStack?
    pattern_symtab <- M.unions <$> mapM collectPat pats
    local (withLocalSymtab pattern_symtab) $ collectExp exp
  LeftSection _l exp qop -> parseSection qop exp
  RightSection _l qop exp -> parseSection qop exp
  Lit _l literal -> parseLiteral literal
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

onlyStmts :: [QualStmt S] -> Parse [Stmt S]
onlyStmts = fmap catMaybes . mapM getStmt
  where
  getStmt qual_stmt = case qual_stmt of
    QualStmt _l stmt -> return $ Just stmt
    other -> do
      warn other "TransformListComp?"
      return Nothing

parseLiteral :: Literal S -> Parse ()
parseLiteral literal = case literal of
  Char _ _ _ -> highlight l $ Literal CharLit
  HSE.String _ _ _ -> highlight l $ Literal StringLit
  Int _ _ _ -> highlight l $ Literal NumLit
  Frac _ _ _ -> highlight l $ Literal NumLit
  _ -> return ()
  where
  l = ann literal

collectAlt :: Alt S -> Parse ()
collectAlt (Alt _l pat guarded_alts mb_binds) = do
  pattern_symtab <- collectPat pat  -- only used locally
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
  PLit _l lit -> parseLiteral lit >> return M.empty
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
  PatTypeSig _l pat ty -> collectType ty >> collectPat pat
  other -> warn other "Pat" >> return M.empty

matchName :: Match S -> String
matchName m = hseSymOrIdentValue $ case m of
  Match _l nm _ _ _ -> nm
  InfixMatch _l _ nm _ _ _ -> nm

-- | Parses the subtree by pushing the Match under definition to the
-- `definitionStack`.
parseMatch :: Maybe SymLoc -> Match S -> Parse SymbolAndLoc
parseMatch mb_reference_loc m = do
  let (name, patterns, rhs, mb_binds) = case m of
        Match _l nm ps r bs -> (nm, ps, r, bs)
        InfixMatch _l p nm ps r bs -> (nm, p:ps, r, bs)
      defined_sym_and_loc = hseNameToSymbolAndLoc CTerm name
  highlight (ann name) $ VariableDecl MatchHead
  case mb_reference_loc of
    Nothing -> return ()
    Just reference_loc -> do
      -- TODO this is a pseudo-reference, annotate Ref (also for pseudo-ref
      --      coming from typesig, instance method, etc...)
      --      Alternatively, the reference now includes definition stack, which
      --      should also be good enough.
      stack <- asks definitionStack
      tell . DList.singleton . LRef $
        Ref (snd defined_sym_and_loc) reference_loc stack
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
    parseTyVarBinds binds
    return $ hseNameToSymbolAndLoc CType hd

parseTyVarBinds :: [TyVarBind S] -> Parse ()
parseTyVarBinds tyvarbinds =
  mapM_ (\b -> highlight (ann b) TypeVariable) tyvarbinds

parseClassDecl :: Symbol -> ClassDecl S -> Parse SymTab
parseClassDecl clsname cdecl = case cdecl of
  ClsDecl _l decl -> do
    decl_symtab <- collectDeclWithSigExports True True decl
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
  TyForall _l mb_tyvarbinds mb_ctx t -> do
    case mb_tyvarbinds of
      Just binds -> parseTyVarBinds binds
      Nothing -> return ()
    warnMay mb_ctx "Context"
    collectType t
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
  -- TODO parens for operator-like funny chars?
  Qual l (ModuleName _ mname) name ->
    Just (l, mname ++ "." ++ hseSymOrIdentValue name)
  UnQual l name ->
    Just (l, hseSymOrIdentValue name)
  Special _ _ ->
    -- TODO ?
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

insertPair = uncurry M.insert
singletonPair = uncurry M.singleton
  
applyIf :: Bool -> (a -> a) -> a -> a
applyIf c f = if c then f else id
