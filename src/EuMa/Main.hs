{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-} -- for CmdArgs

module EuMa.Main where

import System.Console.CmdArgs
import Data.List (intercalate, zip4)
import Data.List.Split (splitWhen)
-- import System.Environment --args <- getArgs
import Control.Monad (unless)
import System.Random.MWC (createSystemRandom)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Csv (encode, Only(..))
import qualified Data.ByteString.Lazy as BS
import EuMa.Types
import EuMa.Pituitary

data SubCommand =
  Peaks Parameters | Curves Parameters
    deriving (Data, Typeable, Show, Eq)

------------------ Simulation parameters -------------------------------------------------------------
-- Default parameter values

paramsInit :: Parameters
paramsInit =
  Parameters { cm = 10        &= help "( 10 pF) Membrane capacitance",
               gcal = 2       &= help "(  2 nS) Maximal conductance of Ca^2+ channels",
               vca = 60       &= help "( 60 mV) Reversal potential for Ca^2+ channels",
               vm = (-20)     &= help "(-20 mV) Voltage value at midpoint of m_inf",
               sm = 12        &= help "( 12 mV) Slope parameter of m_inf",
               gk = 3.2       &= help "(3.2 nS) Maximal conductance of K channels",
               vk = -75       &= help "(-75 mV) Reversal potential for K^+",
               vn = -5        &= help "( -5 mV) Voltage value at midpoint of n_inf",
               sn = 10        &= help "( 10 mV) Slope parameter of n_inf",
               taun = 30      &= help "( 30 ms) Time constant of n",
               gsk = 2        &= help "(  2 nS) Maximal conductance of SK channels",
               ks = 0.4       &= help "(0.4 muM) [Ca] at midpoint of S_inf",
               gbk = 0.5      &= help "(0.5 nS) Maximal conductance of BK channels",
               vf= (-20)      &= help "(-20 mV) Voltage value at midpoint of f_inf",
               sf= 1          &= help "(  1 mV) Slope parameter of f_inf",
               taubk = 5      &= help "(  5 ms) Time constant of f",
               gl = 0.2       &= help "(0.2 nS) Leak conductance",
               vl = (-50)     &= help "(-50 mV) Reversal potential for the leak current",
               noise = 4.0    &= help "(  4 pA) Amplitude of noise current",
               fc = 0.01      &= help "( 0.001) Fraction of free Ca^2+ions in cytoplasm",
               alpha = 0.0015 &= ignore ,
               kc = 0.12      &= help "(0.12 ms^-1) Rate of Ca^2+ extrusion"}

------------------ Model variables -------------------------------------------------------------------
--   dV/dt = dotVar( S )
--       S = state(  V )

-- Initial value for variables
initVar :: Variables Double
initVar = Variables { varV = -60, varn = 0.1, varf = 0.01, varCa = 0.25 }

------------------ Random process --------------------------------------------------------------------

global :: Global
global = Global { stepSize = step, simTime = time, totalSteps = steps, totalSpikes = 100 } where
  step = 0.01
  time = 5000.0
  steps = (floor $ time/step)

--data AllParams = AllParams { parameters :: Parameters, global :: Global }  deriving (Data,Typeable,Show,Eq)

peaks gen parameters = do
  -- more than 2x faster
  let threshold = -35
      compLenSpikes =  if totalSpikes global == 0 then lengthSpikes     (totalSteps  global)
                                                  else lengthSpikesUpTo (totalSpikes global)
  lenSpikes <- runReaderT (compLenSpikes initVar threshold ) (In parameters global gen)

  BS.putStr $ encode $ map Only lenSpikes

curves gen parameters = do
  traj <- runReaderT (simulate (totalSteps global) initVar) $ In parameters global gen

  let
    nPlot         = 5000 :: Int
    nskip         = totalSteps global `div` nPlot
    dtPlot        = (stepSize global)*(fromIntegral nskip)

    everynth k xs = y:(everynth k ys) where y:ys = drop (k-1) xs
    t =  map ((dtPlot*) . fromIntegral) [1..nPlot]
    Variables{..} = sequenceA $ (take nPlot . everynth nskip) traj

  BS.putStr $ encode $ zip4 varV varn varf varCa

main :: IO ()
main = do
     -- ask user file name and parameters to be changed
     --AllParams{..} <- cmdArgs $ (AllParams paramsInit globalInit) -- paramsInit
     subcommand <- cmdArgs $ modes [Peaks paramsInit, Curves paramsInit]
                            &= help helpText
                            &= program "pituitary"
                            &= summary "Pituitary cell electrical dynamic simulator v0.1.0"

     gen  <- createSystemRandom

     case subcommand of
       Peaks params -> peaks gen params
       Curves params -> curves gen params


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

traj <-  runReaderT (simulate (totalSteps global) initVar) $ In (paramsInit {gbk = 0, noise = 100})  global gen
traj1 <- runReaderT (simulate (totalSteps global) initVar) $ In (paramsInit {gbk = 1, noise = 100})  global gen

let xx  = splitWhen  (> -35) (varV <$> traj)
let xx1 = splitWhen  (> -35) (varV <$> traj1)

(stepSize global *) $ mean . fromList . map fromIntegral $  filter (>0) ( map length xx )
(stepSize global *) $ mean . fromList . map fromIntegral $  filter (>0) ( map length xx1)

(stepSize global *) $ stdDev . fromList . map fromIntegral $  filter (>0) ( map length xx )
(stepSize global *) $ stdDev . fromList . map fromIntegral $  filter (>0) ( map length xx1)

mean . fromList $ filter (>0) ( (* (stepSize global / 1000) ) . fromIntegral . length <$> splitWhen  (> -35) (varV <$> traj1) )

onscreen $ histogram ( (* (stepSize global / 1000) ) . fromIntegral <$> ( filter (>5) $ length <$> splitWhen  (< -35) (varV <$> traj)  ) ) 10 @@ [o2 "histtype" "stepfilled" ,o2 "alpha" 0.8, o2 "normed" True] % xlim 0 0.2
onscreen $ histogram ( (* (stepSize global / 1000) ) . fromIntegral <$> ( filter (>5) $ length <$> splitWhen  (< -35) (varV <$> traj1) ) ) 20 % xlim 0 0.2


onscreen $  densityBandwidth ( ( (* (stepSize global / 1000) ) . fromIntegral <$> ( filter (>5) $ length <$> splitWhen  (< -35) (varV <$> traj)   ) )) 0.00005 (Just (0,0.2)) %
            densityBandwidth ( ( (* (stepSize global / 1000) ) . fromIntegral <$> ( filter (>5) $ length <$> splitWhen  (< -35) (varV <$> traj1)  ) )) 0.00005 (Just (0,0.2))

-}

-- let mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

--var xs = (var' 0 0 0 xs) / (fromIntegral $ length xs - 1)
--    where
--      var' _ _ s [] = s
--      var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
--         where
--           delta = x - m
--           nm = m + delta/(fromIntegral $ n + 1)

--std = sqrt . var
