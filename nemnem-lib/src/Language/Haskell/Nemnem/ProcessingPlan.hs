{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Nemnem.ProcessingPlan
  ( dagOrdered
  ) where

import Control.Applicative
import Control.Monad
import Data.Graph.Inductive
import qualified Data.IntMap as IM
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T

data FileData = FileData
  { fdPath :: FilePath
  , fdModule :: T.Text
  , fdImportedModules :: [T.Text]
  }

-- TODO error handling
parseFile :: FilePath -> IO FileData
parseFile path = {-# SCC parseFile #-} do 
  lines <- T.lines <$> T.readFile path
  let import_lines = filter ("import " `T.isPrefixOf`) lines
      imported_modules = nub . map getImportedModule $ import_lines
      module_name = case filter ("module " `T.isPrefixOf`) lines of
        first_line:_ -> case T.words first_line of
          "module":things:_ -> beforeParen things
          _ -> "Unknown"
        _ -> "Unknown"
  return $ FileData path module_name imported_modules    
  where
  getImportedModule line =
    let things = case T.words line of
          "import":"qualified":things:_ -> things
          "import":things:_ -> things
          _ -> error . T.unpack $ "unexpected: " <> line
    in beforeParen things
  beforeParen = fst . T.breakOn "(" 

importGraph :: [FileData] -> [(FilePath, [FilePath])]
importGraph file_datas = {-# SCC importGraph #-}
  let module_paths = M.fromList . map moduleAndPath $ file_datas
  in flip map file_datas $ \fd ->
       let import_paths = catMaybes
             . map (flip M.lookup module_paths)
             . fdImportedModules
             $ fd
       in (fdPath fd, import_paths)
  where
  moduleAndPath fd = (fdModule fd, fdPath fd)

dagOrdered :: [FilePath] -> IO [FilePath]
dagOrdered paths = {-# SCC dagOrdered #-} do
  datas <- mapM parseFile paths
  let adj = importGraph datas
      idmap = M.fromList $ paths `zip` [0..]
  return $ dagOrdered' (fromJust . flip M.lookup idmap) $ adj  

dagOrdered' :: (a -> Int) -> [(a, [a])] -> [a]
dagOrdered' getId imports = {-# SCC dagOrdered' #-}
  let entries = map fst imports
      nodes = map getId entries
      reversed = IM.fromList $ nodes `zip` entries
      edges = [(getId a_imp, getId a) | (a,a_imps) <- imports, a_imp <- a_imps]
      g = mkUGraph nodes edges  :: UGr
  -- TODO what happens in case the input is not a DAG for some evil reason?
  in map (fromJust . flip IM.lookup reversed) . topsort $ g
