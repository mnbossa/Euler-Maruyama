{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module EuMa.Main
  (main, doMain, peaks, curves, multi, mkRandomGenerator, MultiCurveRecord(..))
where

import Data.List (zip5)
-- import System.Environment --args <- getArgs
import System.Random.MWC (createSystemRandom, Gen, initialize)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad (forM)
import Data.Csv (encode, ToRecord(..), toField) --Only(..)
import qualified Pipes.Csv as CSV
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Pipes (runEffect, each, (>->), Consumer)
import qualified Pipes.ByteString as PBS
import qualified Pipes.Prelude as P
import qualified Pipes.Concurrent as PCon
import GHC.Conc (numCapabilities)
import qualified Control.Concurrent.Async as Async
import EuMa.Types
import EuMa.Pituitary
import EuMa.CmdLine

------------------ Simulation parameters -------------------------------------------------------------
-- Default parameter values

------------------ Model variables -------------------------------------------------------------------
--   dV/dt = dotVar( S )
--       S = state(  V )

-- Initial value for variables
initVar :: Variables Double
initVar = Variables { varV = -60, varn = 0.1, varf = 0.01, varCa = 0.25 }

------------------ Random process --------------------------------------------------------------------

peaks :: PrimMonad m => Gen (PrimState m) -> Global -> Parameters -> m Features -- ([Double], [Double], [Double], [Double], [Double])
peaks gen global parameters = runReaderT (computeFeatures (totalSpikes global) initVar ) (In parameters global gen) 

-- fixme: what should return when computing several curves at a time?
peaks1 :: PrimMonad m => Gen (PrimState m) -> Global -> Parameters -> m [Double]
peaks1 gen global param = do
   Oscillating r _ _ _ _ <- peaks gen global param 
   return r

curves :: PrimMonad m => Gen (PrimState m) -> Global -> Parameters -> m ([Double],[Double],[Double],[Double],[Double])
curves gen global parameters = do
  traj <- runReaderT (simulate (totalSteps global) initVar) $ In parameters global gen

  let
    nPlot         = min 5000 (totalSteps global) :: Int
    nskip         = totalSteps global `div` nPlot
    dtPlot        = (stepSize global)*(fromIntegral nskip)

    everynth k xs = y:(everynth k ys) where y:ys = drop (k-1) xs
    t =  map ((dtPlot*) . fromIntegral) [1..nPlot]
    Variables{..} = sequenceA $ (take nPlot . everynth nskip) traj
  return (t, varV, varn, varf, varCa)

data MultiCurveRecord = MCR Parameters [Double]

instance ToRecord MultiCurveRecord where
  toRecord (MCR params xs) = toRecord params <> V.fromList (map toField xs)

-- fixme this just repeats the same parameters multiple times, we need to create
-- a combination of different parameters
mkMultiParameters :: Int -> Parameters -> [Parameters]
mkMultiParameters n = replicate n

multi :: (Foldable f) => Gen (PrimState IO) -> Global -> Consumer MultiCurveRecord IO () -> f Parameters -> IO [Async.Async ()]
multi gen globals process ps = do
  -- we create a channel to communicate the producer and the workers
  (output, input) <- PCon.spawn (PCon.bounded (3*threads))

  -- spawn a single thread to produce all the work, should be enough since
  -- creating parameters is lightweight
  t1 <- Async.async $ runEffect $ each ps >-> PCon.toOutput output

  -- spawn threads workers taking work from the queue
  ts <- forM [1..threads] $ \_ ->
    Async.async $ runEffect $
              PCon.fromInput input
                >-> P.mapM (\p -> MCR p <$> peaks1 gen globals p)
                >-> process

  pure (t1:ts)

  where threads = fromMaybe numCapabilities $ numThreads globals

mkRandomGenerator :: Maybe RandomSeed -> IO (Gen (PrimState IO))
mkRandomGenerator Nothing = createSystemRandom
mkRandomGenerator (Just seed) = initialize seed

doMain :: Options -> IO ()
doMain Options{optCommand = command, optGlobals = globals} = do
  gen  <- mkRandomGenerator (rndSeed globals)

  case command of
    Peaks params -> peaks gen globals params >>= BS.putStr . encodeFeat
    Curves params -> curves gen globals params >>= BS.putStr . encode . unzip5
    MultiCurves total params -> do
      threads <- multi gen globals (CSV.encode >-> PBS.stdout) (mkMultiParameters total params)
      mapM_ Async.wait threads

  where
    unzip5 (t,varV,varn,varf,varCa) = zip5 t varV varn varf varCa
    encodeFeat Oscillating{..} = encode . unzip5 $ ( pptime, amplitude, duration, area, nlocmax)
    encodeFeat Silent{..}      = encode [(meanV, stdV, maxV, minV)]

main :: IO ()
main = parseCmdLine >>= doMain

{-

import Data.List.Split
import Data.List
import Data.Vector (fromList)
import Statistics.Sample

import Options.Applicative
gen  <- createSystemRandom
params <-  execParser (info  parametersParser fullDesc )
global <-  execParser (info  globalParser  fullDesc )

traj <-  runReaderT (simulate (totalSteps global) initVar) $ In (params {gbk = 0, noise = 100})  global gen

-}

