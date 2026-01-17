{-|
Module      : Ctxlint.Types
Description : Core types for Context Spec validation
Copyright   : (c) ctxlint team
License     : MIT

Core data types for representing and validating Context Specs.
-}

module Ctxlint.Types where

import qualified Data.Text as T

-- | Represents an incomplete Context Spec
data IncompleteSpec = IncompleteSpec
  { incContents :: T.Text
  } deriving (Show, Eq)

-- | Represents a complete Context Spec
data CompleteSpec = CompleteSpec
  { comContents :: T.Text
  } deriving (Show, Eq)

-- | Context Spec can be either incomplete or complete
data Spec
  = Incomplete IncompleteSpec
  | Complete CompleteSpec
  deriving (Show, Eq)

-- | Validation error
data ValidationError
  = MissingSection String
  | MalformedSection String
  | InconsistentSpec String
  | OtherError String
  deriving (Show, Eq)

-- | Validation result
type ValidationResult = Either [ValidationError] CompleteSpec
