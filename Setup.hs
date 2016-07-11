import           Distribution.Simple (UserHooks (postReg), defaultMainWithHooks,
                                      simpleUserHooks)
import           System.Exit         (ExitCode (..), exitWith)
import           System.Process      (system)

main :: IO ()
main =
    defaultMainWithHooks $
    simpleUserHooks
    { postReg = hs2purs
    }
  where
    hs2purs _ _ _ _ =
        system "stack $NIX_STACK exec -- rscoin-hs2purs" >>= printError
    printError ExitSuccess = return ()
    printError e = exitWith e
