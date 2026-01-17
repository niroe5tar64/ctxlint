{-|
Module      : Ctxlint.Parser
Description : Markdown parser for Context Specs
Copyright   : (c) ctxlint team
License     : MIT

Parser for reading and extracting Context Spec structure from Markdown.
-}

module Ctxlint.Parser (parseSpec) where

import qualified Data.Text as T
import Ctxlint.Types

-- | Parse a Context Spec from markdown text
parseSpec :: T.Text -> Either String IncompleteSpec
parseSpec contents = do
  -- TODO: Implement Markdown parser using Parsec
  -- - Extract section headers
  -- - Extract section contents
  -- - Build spec structure
  Right $ IncompleteSpec contents
