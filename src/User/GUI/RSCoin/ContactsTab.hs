-- | Contacts tab module
module GUI.RSCoin.ContactsTab
       ( createContactsTab
       , initContactsTab
       ) where

import           GUI.RSCoin.Glade      (GladeMainWindow (..))
import           GUI.RSCoin.GUIAcid    (GUIState)
import           GUI.RSCoin.MainWindow (ContactsTab (..))
import qualified GUI.RSCoin.MainWindow as M

createContactsTab :: GladeMainWindow -> IO ContactsTab
createContactsTab GladeMainWindow{..} =
    return $
    ContactsTab gTreeViewContactsView gButtonAddContact gLabelContactsNum

initContactsTab :: GUIState -> M.MainWindow -> IO ()
initContactsTab _ _ = return ()
