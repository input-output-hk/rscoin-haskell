module RSCoin.Core.Constants
       (period
       ,epoch
       )where

-- Just dummies
second :: Integer
second = 10 ^ (6 :: Integer)

period :: Integer
period = 50 * second

epoch :: Integer
epoch = 5 * second
