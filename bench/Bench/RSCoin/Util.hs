-- | Benchmark related utils.

module Bench.RSCoin.Util
       ( ElapsedTime (..)
       , measureTime
       , measureTime_
       , perSecond
       ) where

import           Data.Text.Buildable (Buildable (build))
import           Formatting          (bprint, shown, (%))
import           System.Clock        (Clock (..), TimeSpec, diffTimeSpec,
                                      getTime, toNanoSecs)

data ElapsedTime = ElapsedTime
    { elapsedCpuTime  :: TimeSpec
    , elapsedWallTime :: TimeSpec
    } deriving (Show)

instance Buildable ElapsedTime where
    build ElapsedTime{..} =
        bprint
            ("(CPU time = " % shown % ", wall time = " % shown)
            elapsedCpuTime
            elapsedWallTime

measureTime :: IO a -> IO (ElapsedTime, a)
measureTime action = do
    cpuTimeBefore <- getTime ProcessCPUTime
    wallTimeBefore <- getTime Realtime
    res <- action
    wallTimeAfter <- getTime Realtime
    cpuTimeAfter <- getTime ProcessCPUTime
    return
        ( ElapsedTime
          { elapsedCpuTime = cpuTimeAfter `diffTimeSpec` cpuTimeBefore
          , elapsedWallTime = wallTimeAfter `diffTimeSpec` wallTimeBefore
          }
        , res)

measureTime_ :: IO a -> IO ElapsedTime
measureTime_ = fmap fst . measureTime

perSecond :: (Real a, Fractional b) => a -> TimeSpec -> b
perSecond n time =
    fromRational $ toRational n / (fromIntegral $ toNanoSecs time) * 1.0e9
