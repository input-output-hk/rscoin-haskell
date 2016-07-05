-- | Constants in full tests.

module Test.RSCoin.Full.Constants
       ( userAddressesCount
       , bankUserAddressesCount
       , minColor
       , maxColor
       ) where

import           RSCoin.Core (Color)

-- | Number of addresses each casual user has in wallet (constant).
userAddressesCount :: Num a => a
userAddressesCount = 5

-- | Number of addresses each bank user has in wallet (constant).
bankUserAddressesCount :: Num a => a
bankUserAddressesCount = 6

-- | Minimal color used in tests.
minColor :: Color
minColor = -1

-- | Maximal color used in tests.
maxColor :: Color
maxColor = 3
