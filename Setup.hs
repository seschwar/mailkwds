#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.FilePath ((</>))
import System.Cmd (rawSystem)

main = defaultMainWithHooks simpleUserHooks { runTests = runTests' }

runTests' _ _ _ lbi = rawSystem ((buildDir lbi) </> "test" </> "test")
                      ["--maximum-generated-tests=1000"] >> return ()

