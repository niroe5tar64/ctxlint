module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: ctxlint <spec-file>"
      exitFailure
    (filepath:_) -> do
      -- TODO: Implement CLI logic
      -- - Read file
      -- - Parse and validate
      -- - Output results
      putStrLn $ "Processing: " ++ filepath
      exitSuccess
