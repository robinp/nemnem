{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Nemnem.Internal.Source
  ( moduleName
  , ModuleOptions(..)
  , moduleOptions
  , beforeOpenParen
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- TODO these functions should receive streamed input
--      to save reading all the file.

-- | Parses the module name of a .hs source.
moduleName :: [Text] -> Maybe Text
moduleName src_lines = {-# SCC moduleName #-}
  case filter ("module " `T.isPrefixOf`) src_lines of
    first_line:_ -> case T.words first_line of
      "module":things:_ -> Just $ beforeOpenParen things
      _ -> Nothing
    _ -> Nothing

data ModuleOptions = ModuleOptions
  { moGlasgowExts :: Bool
  }

moduleOptions :: [Text] -> ModuleOptions
moduleOptions src_lines = {-# SCC moduleOptions #-}
  -- TODO this is not 100% accurate, improve if needed
  let matches = filter ("{-# OPTIONS -fglasgow-exts" `T.isPrefixOf`) src_lines
  in ModuleOptions . not . null $ matches

-- | Returns the portion of the text before any opening parenthesis.
beforeOpenParen :: Text -> Text
beforeOpenParen = fst . T.breakOn "(" 

