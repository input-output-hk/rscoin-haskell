import           AcidState (closeState, openState)

main :: IO ()
main = do
    st <- openState "ololo"
    closeState st
