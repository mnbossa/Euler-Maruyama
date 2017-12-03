{-# LANGUAGE RecordWildCards #-}

module EuMa.Pituitary
  ( Comp
  , Env(In)
  , lengthSpikes
  , lengthSpikesUpTo'
  , simulate

-- we don't really need to export dotVar, but because of inlineing we *need* to export it
-- to avoid a big performance penalty
  , dotVar
  ) where

import Control.Monad.Trans.Reader (ReaderT, ask)
import System.Random.MWC (Gen)
import System.Random.MWC.Distributions (standard)
import Control.Monad.Primitive (PrimState, PrimMonad)

import EuMa.Types


data Env m = In Parameters Global (Gen (PrimState m))
type Comp m a = ReaderT (Env m) m a


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

wiener :: (PrimMonad m) => Double -> Double -> Gen (PrimState m) -> m Double
wiener x dt gen = (* step) <$>  standard gen
  where step = x / (dt ** 0.5)
{-# INLINABLE wiener #-}

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
{-# INLINABLE state #-}

-- set of equations that describe the dynamics of model variables in terms of state variables
dotVar :: (PrimMonad m) => Variables Double -> Comp m (Variables Double)
dotVar v@Variables{..} = do
     In Parameters{..} _ _ <- ask
     State{..} <- state v
     return Variables { varV  = -(stIca + stIk + stISK + stIBK + stIleak + stInoise)/cm
                      , varn  = (stninf - varn)/taun
                      , varf  = (stfinf - varf)/taubk
                      , varCa = -fc*(alpha*stIca + kc*varCa) }
-- notice we need to force inlineing for this one, ghc seems to have the wrong heuristic
-- without inlining here performance degrades 10x
{-# INLINE dotVar #-}

-------------- Euler–Maruyama method to estimate the stochastic differential equation (SDE) ----------
eulerStep :: (PrimMonad m) => Variables Double -> Comp m (Variables Double)
eulerStep v = do
        In _ Global{..} _ <- ask
        dv <- dotVar v
        return $ (+) . (*stepSize) <$> dv <*> v
     -- return $ liftA2 (+) v (fmap (stepSize*) dv) -- the same
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

-- drop the first 3 spikes to skip transient effects
lengthSpikes :: (PrimMonad m) => Int -> Variables Double -> Double -> Comp m [Int]
lengthSpikes  n x0 th = comph' (n+3) x0 [0] where
  comph' _ _ [] = error "unexpected pattern in lengthSpikes"
  comph' m x hh@(h:hs) = do
    new <- eulerStep x
    let hhh | v>=th        =  (h+1):hs
            | v<th && h==0 = hh
            | v<th && h>0  = 0:hh
            | otherwise = error "unexpected pattern in lengthSpikes"
            where v = varV new
    if m==1 then return (take n (clean hhh)) else comph' (m-1) new hhh
       where clean yy@(y:ys) = if  y == 0 then ys else yy
             clean [] = error  "unexpected pattern in lengthSpikes"
{-# INLINABLE lengthSpikes #-}

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



-- drop the first 3 spikes to skip transient effects
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
{-# INLINABLE lengthSpikesUpTo #-}
