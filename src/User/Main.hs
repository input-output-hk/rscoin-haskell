import qualified UserOptions as O

main :: IO ()
main = do
    opts <- O.getUserOptions
    putStrLn $ "Program called with the following options: \n" ++ show opts
