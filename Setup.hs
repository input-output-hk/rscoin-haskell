import           Distribution.Simple (defaultMainWithHooks, simpleUserHooks,
                                      UserHooks (postBuild))
import           System.Process      (system)
import           System.Exit         (exitWith, ExitCode (..))

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { postBuild = hs2purs }
  where hs2purs _ _ _ _ =
            system "stack $NIX_STACK exec -- rscoin-hs2purs" >>= printError
        printError ExitSuccess = return ()
        printError e           = exitWith e
