-- | Contacts tab module
module GUI.RSCoin.ContactsTab
       ( createContactsTab
       , initContactsTab
       ) where

import           GUI.RSCoin.Glade      (GladeMainWindow (..))
import           GUI.RSCoin.MainWindow (ContactsTab (..))
import qualified GUI.RSCoin.MainWindow as M

createContactsTab :: GladeMainWindow -> IO ContactsTab
createContactsTab GladeMainWindow{..} =
    return $
    ContactsTab gTreeViewContactsView gButtonAddContact gLabelContactsNum

initContactsTab :: M.MainWindow -> IO ()
initContactsTab = const $ return ()
