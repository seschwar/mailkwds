#!/usr/bin/env runhaskell

module Main where

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import System.FilePath ((</>))
import System.Cmd (rawSystem)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { runTests            = runTests'
    , hookedPreProcessors = [("md", ppPandoc), ("text", ppPandoc)]
    }

runTests' _ _ _ lbi = rawSystem (buildDir lbi </> "test" </> "test")
                      ["--maximum-generated-tests=1000"] >> return ()

ppPandoc _ _ = PreProcessor
    { platformIndependent = True
    , runPreProcessor     = mkSimplePreProcessor $ \input output verbosity -> do
--      info verbosity (input ++ " preprocessed to " ++ output)
        rawSystem "pandoc" ["-sS" ,"--from=markdown", "--to=man", "--output",
                            output, input]
        return ()
    }
