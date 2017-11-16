{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module EuMa.Main where

import Data.List (zip5)
-- import System.Environment --args <- getArgs
import System.Random.MWC (createSystemRandom, Gen)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Csv (encode, Only(..), ToRecord(..), toField)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.Monoid ((<>))
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

--data AllParams = AllParams { parameters :: Parameters, global :: Global }  deriving (Data,Typeable,Show,Eq)
peaks :: PrimMonad m => Gen (PrimState m) -> Global -> Parameters-> m [Int]
peaks gen global parameters = do
  -- more than 2x faster
  let threshold = -35
      compLenSpikes =  if totalSpikes global == 0 then lengthSpikes     (totalSteps  global)
                                                  else lengthSpikesUpTo (totalSpikes global)
  runReaderT (compLenSpikes initVar threshold ) (In parameters global gen)

curves :: PrimMonad m => Gen (PrimState m) -> Global -> Parameters -> m ([Double],[Double],[Double],[Double],[Double])
curves gen global parameters = do
  traj <- runReaderT (simulate (totalSteps global) initVar) $ In parameters global gen

  let
    nPlot         = 5000 :: Int
    nskip         = totalSteps global `div` nPlot
    dtPlot        = (stepSize global)*(fromIntegral nskip)

    everynth k xs = y:(everynth k ys) where y:ys = drop (k-1) xs
    t =  map ((dtPlot*) . fromIntegral) [1..nPlot]
    Variables{..} = sequenceA $ (take nPlot . everynth nskip) traj
  return (t, varV, varn, varf, varCa)

data MultiCurveRecord = MCR Parameters [Int]

instance ToRecord MultiCurveRecord where
  toRecord (MCR params xs) = toRecord params <> V.fromList (map toField xs)

main :: IO ()
main = do
     -- ask user file name and parameters to be changed
     Options{optCommand = command, optGlobals = globals} <- parseCmdLine

     gen  <- createSystemRandom

     case command of
       Peaks params -> peaks gen globals params >>= BS.putStr . encode . map Only
       Curves params -> curves gen globals params >>= BS.putStr . encode . uzip5
       MultiCurves total params ->
         mapM (\p -> (p,) <$> peaks gen globals p) (paramList total params) >>= BS.putStr . encode . fmap (uncurry MCR)

  where
    uzip5 (t,varV,varn,varf,varCa) = zip5 t varV varn varf varCa
    paramList total params = replicate total params --fixme generate a random list of parameters instead


-- fixme move this to the CmdLine module
helpText :: String
helpText = "\
\Generate predictions from a stochastic model for the activity of a pituitary \
\lactotroph as described in the paper Fast-Activating Voltage- and \
\Calcium-Dependent Potassium (BK) Conductance Promotes Bursting in Pituitary \
\Cells, J. Tabak, M. Tomaiuolo, A.E. Gonzalez-Iglesias, L.S. Milescu, R. \
\Bertram, the Journal of Neuroscience, 31:16855-16863, 2011. \n\
\\n  Usage:\n    ./pituitary file [OPTIONS]     \n   Produce 4 .png files \
\(the images of each variable trajectory vs. time) and a .txt file (spike \
\lengths: number of time-step) using a fixed simulation time.\n \n \n\
\   ./pituitary file.fig [OPTIONS] \n Produce 4 .fig files as above, \
\but where fig is one of the following: png, pdf, jpg, jpeg. No txt \
\file is produced.\n \n    ./pituitary file.txt [OPTIONS] \n \
\ Produce only txt file. Simulation is run until a fixed number of spikes \
\is obtained."


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

