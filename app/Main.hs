module Main where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Foldable

import P
import Parser
import Lang

----

main = do
  let pth = "testcode/play"
  src <- readFile pth

  runP_IO fileP src >>= \case
    Nothing ->
      putStrLn "<parse error>"
    Just (not_parsed, decl) -> do
      when (not (null not_parsed)) do
        putStrLn "-- not parsed --"
        putStrLn not_parsed

      putStrLn "----- output -----"
      st <- runWithOutput decl

      putStrLn "\n----- context -----\n"
      traceΓ (st);

