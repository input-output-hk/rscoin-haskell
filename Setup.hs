import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           System.Process
import           System.Exit

import           Control.Monad (void)

main = defaultMainWithHooks $ simpleUserHooks { postBuild = buildPureScript }

buildPureScript _ _ _ _ = void $ system "touch bla"
