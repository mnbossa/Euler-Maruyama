{-# LANGUAGE DeriveDataTypeable #-} -- for CmdArgs
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad.Trans.Reader
import System.Random.MWC
import System.Random.MWC.Distributions
import Control.Monad.Primitive
import System.Console.CmdArgs
import Data.List (zip5)
import Data.Csv (encode, Only(..))
import qualified Data.ByteString.Lazy as BS
import Data.Word (Word32)
import Data.Vector (singleton)

------------------ Simulation parameters -------------------------------------------------------------
data Parameters =
  Parameters { mode :: String
             , rndSeed :: Maybe Int
             , cm :: Double      -- (pF) Membrane capacitance
             , gcal :: Double    -- (nS) Maximal conductance of Ca^2+ channels
             , vca :: Double     -- (mV) Reversal potential for Ca^2+ channels
             , vm :: Double      -- (mV) Voltage value at midpoint of m_\inf
             , sm :: Double      -- (mV) Slope parameter of m_\inf
             , gk :: Double      -- (nS) Maximal conductance of K channels
             , vk :: Double      -- (mV) Reversal potential for K^+
             , vn :: Double      -- (mV) Voltage value at midpoint of n_\inf
             , sn :: Double      -- (mV) Slope parameter of n_\inf
             , taun :: Double    -- (ms) Time constant of n
             , gsk :: Double     -- (nS) Maximal conductance of SK channels
             , ks :: Double      -- (\mu M) [Ca] at midpoint of S_\inf
             , gbk :: Double     -- (nS) Maximal conductance of BK channels
             , vf  :: Double     -- (mV) Voltage value at midpoint of f_\inf
             , sf :: Double      -- (mV) Slope parameter of f_\inf
             , taubk :: Double   -- (ms) Time constant of f
             , gl :: Double      -- (nS) Leak conductance
             , vl :: Double      -- (mV) Reversal potential for the leak current
             , noise :: Double   -- (pA) Amplitude of noise current
             , fc :: Double      -- Fraction of free Ca^2+ions in cytoplasm
             , alpha :: Double   -- (\mu M fC^-1) Conversion from charges to molar concentration
             , kc :: Double      -- (ms^-1) Rate of Ca^2+ extrusion
             } deriving (Data,Typeable,Show,Eq)

-- Default parameter values
paramsInit :: Parameters
paramsInit =
  Parameters { mode = "peaks" &= typ "Mode: peaks or curves" &= argPos 0 ,
               rndSeed = Nothing &= help "Seed for random number generator",    
               cm = 10        &= help "( 10 pF) Membrane capacitance",
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

data Variables a = Variables { varV  :: !a -- membrane potential
                             , varn  :: !a -- activation of I_K
                             , varf  :: !a -- activation of I_BK
                             , varCa :: !a -- intracellular Ca^2+ concentration
                             } deriving Show 

instance Functor Variables where
  fmap g t = Variables { varV  = g (varV t)
                       , varn  = g (varn t)
                       , varf  = g (varf t)
                       , varCa = g (varCa t)}

instance Applicative Variables where
  pure x = Variables x x x x
  f <*> v = Variables { varV  = (varV  f) (varV  v)
                      , varn  = (varn  f) (varn  v)
                      , varf  = (varf  f) (varf  v)
                      , varCa = (varCa f) (varCa v) }

-- Initial value for variables
initVar :: Variables Double
initVar = Variables { varV = -60, varn = 0.1, varf = 0.01, varCa = 0.25 }

------------------ Random process --------------------------------------------------------------------
wiener :: (PrimMonad m) => Double -> Double -> Gen (PrimState m) -> m Double
wiener x dt gen = (* step) <$>  standard gen
  where step = x / (dt ** 0.5)

------------------ State variables S (mostly currents) -----------------------------------------------
data State = State { stIca    :: Double
                   , stIk     :: Double
                   , stninf   :: Double
                   , stISK    :: Double
                   , stIBK    :: Double
                   , stfinf   :: Double
                   , stIleak  :: Double
                   , stInoise :: Double
                   }

data Env m = In Parameters Global (Gen (PrimState m))
type Comp m a = ReaderT (Env m) m a

-- State variables are a function of model variables
state :: (PrimMonad m) => Variables Double -> Comp m State
state  Variables{..} = do
  In Parameters{..} Global{..} gen  <- ask
  eta <- wiener noise stepSize gen
  let invminf = 1 + exp ((vm-varV)/sm)
      sinf = varCa**2/(varCa**2+ks**2)

  return State { stIca    = gcal/invminf*(varV - vca)
               , stIk     = gk*varn*(varV - vk)
               , stninf   = 1/(1 + exp ((vn-varV)/sn))
               , stISK    = gsk*sinf*(varV-vk)
               , stIBK    = gbk*varf*(varV - vk)
               , stfinf   = 1/(1 + exp ((vf - varV)/sf))
               , stIleak  = gl*(varV - vl)
               , stInoise = eta
               }

-- set of equations that describe the dynamics of model variables in terms of state variables
dotVar :: (PrimMonad m) => Variables Double -> Comp m (Variables Double)
dotVar v@Variables{..} = do
     In Parameters{..} _ _ <- ask
     State{..} <- state v
     return Variables { varV  = -(stIca + stIk + stISK + stIBK + stIleak + stInoise)/cm
                      , varn  = (stninf - varn)/taun
                      , varf  = (stfinf - varf)/taubk
                      , varCa = -fc*(alpha*stIca + kc*varCa) }

-------------- Euler–Maruyama method to estimate the stochastic differential equation (SDE) ----------
eulerStep :: (PrimMonad m) => Variables Double -> Comp m (Variables Double)
eulerStep v = do
        In _ Global{..} _ <- ask
        dv <- dotVar v
        return $ (+) . (*stepSize) <$> dv <*> v
     -- return $ liftA2 (+) v (fmap (stepSize*) dv) -- the same

iterateMn :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateMn n f x0 = do
    new <- f x0
    if n==1 then return [new] else (new:) <$> iterateMn (n-1) f new
    -- (new:) <$> iterateM f new -- nicer, but Random.MWC is not lazy

simulate :: (PrimMonad m) => Int -> Variables Double -> Comp m [Variables Double]
simulate n = iterateMn n eulerStep

lengthSpikes :: (PrimMonad m) => Int -> Variables Double -> Double -> Comp m [Int]
lengthSpikes  n x0 th = comph' (n+3) x0 [0] where
  comph' _ _ [] = error "unexpected pattern in lengthSpikes"
  comph' m x hh@(h:hs) = do
    new <- eulerStep x
    let hhh | v>=th        =  (h+1):hs
            | v<th && h==0 = hh
            | v<th && h>0  = 0:hh
            | otherwise = error "unexpected pattern in lengthSpikesUpTo"
            where v = varV new
    if m==1 then return (take n (clean hhh)) else comph' (m-1) new hhh
       where clean yy@(y:ys) = if  y == 0 then ys else yy
             clean [] = error  "unexpected pattern in lengthSpikes"


lengthSpikesUpTo :: (PrimMonad m) => Int -> Variables Double -> Double -> Comp m [Int]
lengthSpikesUpTo n x0 th = comph' x0 [0] where
  comph' _ [] = error "unexpected pattern in lengthSpikesUpTo"
  comph' x hh@(h:hs) = do
    new <- eulerStep x
    let hhh | v>=th        =  (h+1):hs
            | v<th && h==0 = hh
            | v<th && h>0  = 0:hh
            | otherwise = error "unexpected pattern in lengthSpikesUpTo"
            where v = varV new
    if length hhh == (n+3) then return (take n (clean hhh)) else comph' new hhh
       where clean yy@(y:ys) = if  y == 0 then ys else yy
             clean [] = error  "unexpected pattern in lengthSpikes"

-- Fix me: compute m1 and m2 outside comph'
lengthSpikesUpTo' :: (PrimMonad m) => Int -> Variables Double ->  Comp m [Int]
lengthSpikesUpTo' n x0 = comph' x0 0 [0] 10000 (-10000) where
  comph' _ _ [] _ _ = error "unexpected pattern in lengthSpikesUpTo"
  comph' x t hh@(h:hs) m1 m2 = do
    In _ Global{..} _ <- ask
    new <- eulerStep x
    let v = varV new
        transient = fromIntegral t * stepSize < 1000
        m1' = if transient then min m1 v else m1    
        m2' = if transient then max m2 v else m2    
        hhh | transient          = hh
            | v >= th1           = (h+1):hs
            | v >= th2 && h > 0  = (h+1):hs
            | v <  th2 && h > 0  = 0:hh
            | otherwise          = hh
            where th1 = m1 + (m2-m1)*0.5
                  th2 = m1 + (m2-m1)*0.2
    if length hhh == (n+3) then return (take n (clean hhh)) else comph' new (t+1) hhh m1' m2'
       where clean yy@(y:ys) = if  y == 0 then ys else yy
             clean [] = error  "unexpected pattern in lengthSpikes"

-------------- Global (simulation) parameters --------------------------------------------------------
data Global = Global { stepSize    :: Double
                     , simTime     :: Double
                     , totalSteps  :: Int
                     , totalSpikes :: Int
                     } --deriving (Data,Typeable,Show,Eq)

global :: Global
global = Global { stepSize = step, simTime = time, totalSteps = steps, totalSpikes = 100 } where
  step = 0.01
  time = 5000.0
  steps = (floor $ time/step)

--data AllParams = AllParams { parameters :: Parameters, global :: Global }  deriving (Data,Typeable,Show,Eq)



mkRandomGenerator :: Maybe Int -> IO (Gen (PrimState IO))
mkRandomGenerator Nothing = createSystemRandom
mkRandomGenerator (Just seed) = initialize $ singleton ( fromIntegral seed :: Word32)

main :: IO ()
main = do

     let helpText = "Generate predictions from a stochastic model for the activity of a pituitary lactotroph as described in the paper " ++
                    "\"Fast-Activating Voltage- and Calcium-Dependent Potassium (BK) Conductance Promotes Bursting in Pituitary Cells\", " ++
                    "J. Tabak, M. Tomaiuolo, A.E. Gonzalez-Iglesias, L.S. Milescu, R. Bertram, the Journal of Neuroscience, 31:16855-16863, 2011." ++
                    "\n \nUsage:\n    ./lactotroph-model curves [OPTIONS]     \n Produce CSV like output containing trajectories of the variables. The simulation time is fixed.\n" ++
                             "\n \n    ./lactotroph-model peaks [OPTIONS] \n Produce a stream of spike lengths. Simulation is run until a fixed number of spikes is obtained."
     -- ask user file name and parameters to be changed
     --AllParams{..} <- cmdArgs $ (AllParams paramsInit globalInit) -- paramsInit
     parameters <- cmdArgs $ paramsInit
                             &= help helpText
                             &= program "pitt-cells"
                             &= summary "Pituitary cell electrical dynamic simulator v0.1.0"

     gen  <- mkRandomGenerator (rndSeed parameters)

     
     if mode parameters == "peaks" then
       do
         -- more than 2x faster
         --let threshold = -40
         --    compLenSpikes =  if totalSpikes global == 0 then lengthSpikes     (totalSteps  global) 
         --                                                else lengthSpikesUpTo (totalSpikes global)
         --lenSpikes <- runReaderT (compLenSpikes initVar threshold ) (In parameters global gen)
         lenSpikes <- runReaderT (lengthSpikesUpTo' (totalSpikes global) initVar ) (In parameters global gen)

         BS.putStr $ encode $ map Only lenSpikes
      else
       do
         traj <- runReaderT (simulate (totalSteps global) initVar) $ In parameters global gen

         let 
           nPlot         = min 5000 (totalSteps global) :: Int
           nskip         = totalSteps global `div` nPlot
           dtPlot        = (stepSize global)*(fromIntegral nskip)
           
           everynth k xs = y:(everynth k ys) where y:ys = drop (k-1) xs
           t =  map ((dtPlot*) . fromIntegral) [1..nPlot]
           Variables{..} = sequenceA $ (take nPlot . everynth nskip) traj

         BS.putStr $ encode $ zip5 t varV varn varf varCa


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
