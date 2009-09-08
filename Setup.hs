#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.FilePath ((</>))
import System.Cmd (rawSystem)

main = defaultMainWithHooks simpleUserHooks { runTests = runTests' }

runTests' _ _ _ lbi = rawSystem testprog [] >> return ()
    where testprog = (buildDir lbi) </> "test" </> "test"

