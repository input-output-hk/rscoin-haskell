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
    hs2purs _ _ _ _ = do
        system "stack $NIX_STACK exec -- rscoin-hs2purs" >>= printError
        -- FIXME: https://gitlab.serokell.io/rscoin/rscoin/commit/fc8d36123dba122a4ce41053e1881ea5e1873030#note_1193
        -- system
        --     "cd block-explorer; PATH=$PATH:node_modules/.bin; npm install; bower install; cd websocket-example; PATH=$PATH:../node_modules/.bin; bower install; pulp test" >>=
        --     printError
    printError ExitSuccess = return ()
    printError e = exitWith e
