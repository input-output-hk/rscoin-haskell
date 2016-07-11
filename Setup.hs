import           Distribution.Simple (defaultMainWithHooks, simpleUserHooks, UserHooks (postBuild))
import           System.Process      (system)

import           Control.Monad       (void)

main = defaultMainWithHooks $ simpleUserHooks { postBuild = buildPureScript }

buildPureScript _ _ _ _ = void $ system "touch bla"
