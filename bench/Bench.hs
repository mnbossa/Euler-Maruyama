module Main where

import Data.Maybe (fromJust)
import qualified Data.Vector as V
import EuMa.CmdLine
import EuMa.Types
import qualified EuMa.Main as M
import Criterion.Main
import qualified Pipes.Prelude as P
import qualified Control.Concurrent.Async as Async
import System.Random.MWC (GenIO)
import Control.DeepSeq (NFData)


main :: IO ()
main = do

  defaultMain [
        benchIt "peaks" peakOptions $ \globals (Peaks params) gen ->
            M.peaks gen globals params

      , benchIt "curves" curvesOptions $ \globals (Curves params) gen ->
            M.curves gen globals params

      , benchIt "multi" multiOptions $
          let consumer = P.mapM_ $ \(M.MCR _ xs) -> seq (sum xs) (pure ())
          in \globals (MultiCurves n params) gen -> do
                threads <- M.multi gen globals consumer (replicate n params)
                mapM_ Async.wait threads
      ]

benchIt :: NFData a => String -> Options -> (Global -> Command -> GenIO -> IO a) -> Benchmark
benchIt name options f =
  bench name $ nfIO $ do
    gen <- M.mkRandomGenerator $ Just (V.singleton 42)
    f globals command gen
  where
    Options{optGlobals = globals, optCommand = command} = options

peakOptions :: Options
peakOptions = fromJust $ stringsToOptions ["peaks", "--totalSpikes", "5"]

curvesOptions :: Options
curvesOptions = fromJust $ stringsToOptions ["curves"]

multiOptions :: Options
multiOptions = fromJust $ stringsToOptions ["multi","--numCurves","8"]
