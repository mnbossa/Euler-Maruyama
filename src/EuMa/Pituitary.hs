{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module EuMa.Pituitary
  ( Comp
  , Env(In)
  , computeFeatures
  , simulate
-- for profiling
  , applyM
  , wiener
  , state
  , eulerStep
  , iterateMn
  , trackSilent
  , compSilent
  , compOscill
  , getPeakFeatures
-- we don't really need to export dotVar, but because of inlineing we *need* to export it
-- to avoid a big performance penalty
  , dotVar
  ) where

import Control.Monad.Trans.Reader (ReaderT, ask)
import System.Random.MWC (Gen)
import System.Random.MWC.Distributions (standard)
import Control.Monad.Primitive (PrimState, PrimMonad)
import Control.Monad ((>=>))
import Data.List (unzip4)

import EuMa.Types

data Env m = In Parameters Global (Gen (PrimState m))
type Comp m a = ReaderT (Env m) m a

-- State variables S (mostly currents)
data State = State { stICa    :: Double -- voltage-gated Ca^2+ current
                   , stIK     :: Double -- delayed rectifier K^+ current
                   , stISK    :: Double -- Ca^2+-activated K^+ current
                   , stIKir   :: Double -- inward rectifying K^+ current
                   , stIBK    :: Double -- a BK-type K^+ current
                   , stIA     :: Double -- A-type K^+ current
                   , stIL     :: Double -- leak current
                   , stInoise :: Double -- stochastic current reflecting channel noise
                   , ninf     :: Double
                   , binf     :: Double
                   , hinf     :: Double
                   }

wiener :: (PrimMonad m) => Double -> Double -> Gen (PrimState m) -> m Double
wiener x dt gen = (* step) <$>  standard gen
  where step = x / (dt ** 0.5)
{-# INLINABLE wiener #-}

-- State variables are a function of model variables
state :: (PrimMonad m) => Variables Double -> Comp m State
state  Variables{..} = do
  In Parameters{..} Global{..} gen  <- ask
  eta <- wiener noise stepSize gen
  let xinf v s = 1/ (1 + exp ((v-varV)/s))
      sinf = varCa**2/(varCa**2+ks**2)
      minf = xinf vm sm
      kinf = xinf vk sk
      ainf = xinf va sa
  return State { stICa    = gca * minf * (varV - eca)
               , stIK     = gk  * varn * (varV - ek)
               , stISK    = gsk * sinf * (varV - ek)
               , stIKir   = gkir* kinf * (varV - ek)
               , stIBK    = gbk * varb * (varV - ek)
               , stIA     = ga  * ainf * varh * (varV - ek)
               , stIL     = gl  * (varV - el)
               , stInoise = eta
               , ninf     = xinf vn sn
               , binf     = xinf vb sb
               , hinf     = xinf vh sh
               }
{-# INLINABLE state #-}

-- set of equations that describe the dynamics of model variables in terms of state variables
dotVar :: (PrimMonad m) => Variables Double -> Comp m (Variables Double)
dotVar v@Variables{..} = do
     In Parameters{..} _ _ <- ask
     State{..} <- state v
     return Variables { varV  = -(stICa + stIK + stISK + stIKir + stIBK + stIA + stIL + stInoise)/cm
                      , varn  = (ninf - varn)/taun
                      , varb  = (binf - varb)/taubk
                      , varh  = (hinf - varh)/tauh
                      , varCa = -fc*(alpha*stICa + kc*varCa) }
-- notice we need to force inlineing for this one, ghc seems to have the wrong heuristic
-- without inlining here performance degrades 10x
{-# INLINE dotVar #-}

-- Euler–Maruyama method to estimate the stochastic differential equation (SDE)
eulerStep :: (PrimMonad m) => Variables Double -> Comp m (Variables Double)
eulerStep v = do
        In _ Global{..} _ <- ask
        dv <- dotVar v
        return $ (+) . (*stepSize) <$> dv <*> v
{-# INLINABLE eulerStep #-}

iterateMn :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateMn n f x0 = do
    new <- f x0
    if n==1 then return [new] else (new:) <$> iterateMn (n-1) f new
    -- (new:) <$> iterateM f new -- nicer, but Random.MWC is not lazy
{-# INLINABLE iterateMn #-}

simulate :: (PrimMonad m) => Int -> Variables Double -> Comp m [Variables Double]
simulate n = iterateMn n eulerStep
{-# INLINABLE simulate #-}

trackSilent :: PrimMonad m => Int -> Variables Double -> Comp m Features
trackSilent m y = trackSilent' y m (0::Double) (0::Double) (1000::Double) (-1000::Double) where
  trackSilent' x n !s !s2 !m0 !m1 = do
    new <- eulerStep x
    let v = varV new
        s'  = s + v
        s2' = s2 + v^(2 :: Int)
        m0' = min m0 v
        m1' = max m1 v
    if n==0 then return $ Silent s' s2' m0' m1' else trackSilent' new (n-1) s' s2' m0' m1' 
{-# INLINABLE trackSilent #-}

compSilent :: (PrimMonad m) => Variables Double -> Int -> Comp m Features
compSilent x n = do
  Silent{..}  <- trackSilent n x
  let m = meanV / fromIntegral n
      v = stdV / fromIntegral n - m^(2 :: Int)
  return $ Silent m (sqrt v) minV maxV
{-# INLINABLE compSilent #-}

getPeakFeatures :: (PrimMonad m) => Variables Double -> Double -> Double -> Comp m (Variables Double, Int, Int, Double, Double)
getPeakFeatures y th1 th2 = compfeat y 0 0 th2 th1 0 where
  compfeat x h0 h1 m0 m1 a = do
    new <- eulerStep x
    let v = varV new
        m0' = max v m0
        m1' = min v m1
        a' = a + max 0 (v - th2)
    let res | v>=th1         = compfeat new (h0+1) (h1+1) m0' m1' a' -- peak starts: count peak duration
            | v>=th2 && h0>0 = compfeat new (h0+1) (h1+1) m0' m1' a'  -- inside peak: add to duration
            | v< th2 && h0>0 = return (new, h0, h1, m0' - m1', a')   -- peak ends: start over both counts
            | otherwise      = compfeat new h0 (h1+1) m0' m1' a'  -- outside peak: count duration between peaks
    res
{-# INLINABLE getPeakFeatures #-}

-- FIXME: compute the rest of the features
compOscill :: (PrimMonad m) => Variables Double -> Double -> Double -> Comp m Features
compOscill x0 th1 th2 = do
  In _ Global{..} _ <- ask
  let steps2time = map ((stepSize *) . fromIntegral)
      sum2area = map ((stepSize *))
  (peakLengthCounts, btwPeakCounts, amplitude, area) <- unzip4 <$> comph' x0 totalSpikes ([] :: [(Int, Int, Double, Double)])
  let zeros = replicate totalSpikes 0
  return $ Oscillating (steps2time peakLengthCounts) (steps2time btwPeakCounts) amplitude (sum2area area) zeros
  where comph' x n hh = do
           (new, h0, h1, dv, a) <- getPeakFeatures x th1 th2
           if length hh == (n+1) then return (take n hh) else comph' new n ((h0, h1, dv, a):hh)
{-# INLINABLE compOscill #-}

amplitudFirst :: PrimMonad m => Int -> Variables Double -> Comp m (Variables Double, Double, Double)
amplitudFirst m y = trackAmplitud y m (1000 :: Double) (-1000 :: Double) where
  trackAmplitud x !n !m0 !m1 = do
    new <- eulerStep x
    let v = varV new
        m0' = min m0 v
        m1' = max m1 v
    if n==0 then return (new, m0', m1') else trackAmplitud new (n-1) m0' m1' 
{-# INLINABLE amplitudFirst #-}

applyM :: Monad m => Int -> (a -> m a) -> a -> m a
applyM n f = foldr (>=>) return (replicate n f)
{-# INLINABLE applyM #-}

computeFeatures :: (PrimMonad m) => Variables Double ->  Comp m Features
computeFeatures x0 = do
  In _ Global{..} _ <- ask
  let sec2n s = round $ s*1000.0/stepSize
      dropFirst s  = applyM (sec2n s) eulerStep
  -- drop first 5 seconds, compute max V and min V during the next 5 seconds
  (new, m0, m1) <- amplitudFirst (sec2n 5) =<< dropFirst 5 x0
  let th1 = m0 + (m1-m0)*0.5
      th2 = m0 + (m1-m0)*0.2
  -- compute features from next n peaks if range of V is larger than 20 mV
  -- or signal statistics (mean, max, etc.) during 5 seconds
  if m1-m0 < 20 then compSilent new (sec2n 5) else compOscill new th1 th2
{-# INLINABLE computeFeatures #-}

