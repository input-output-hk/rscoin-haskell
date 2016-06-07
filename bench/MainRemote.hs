import qualified Turtle as T

bankCommand :: (Monoid s, T.IsString s) => s
bankCommand = mconcat [ "cd \"$HOME/rscoin\"\n"
                      , "git pull --ff-only\n"
                      , "stack build rscoin\n"
                      , "stack exec -- rscoin-bank serve\n"
                      ]

main :: IO ()
main = do
    T.procStrict
        "ssh"
        [ "-i"
        , "/home/gromak/.ssh/rscointest.pem"
        , "ubuntu@52.58.26.146"
        , bankCommand]
        mempty
