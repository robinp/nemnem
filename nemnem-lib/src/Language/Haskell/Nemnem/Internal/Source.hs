{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Nemnem.Internal.Source
  ( moduleName
  , beforeOpenParen
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Parses the module name of a .hs source.
moduleName :: [Text] -> Maybe Text
moduleName src_lines =
  case filter ("module " `T.isPrefixOf`) src_lines of
    first_line:_ -> case T.words first_line of
      "module":things:_ -> Just $ beforeOpenParen things
      _ -> Nothing
    _ -> Nothing

-- | Returns the portion of the text before any opening parenthesis.
beforeOpenParen :: Text -> Text
beforeOpenParen = fst . T.breakOn "(" 

