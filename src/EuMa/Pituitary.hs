{-# LANGUAGE RecordWildCards #-}

module EuMa.Pituitary where

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
  where step = x * (dt ** 0.5)

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
lengthSpikes  n x0 th = comph' n x0 [0] where
  comph' _ _ [] = error "unexpected pattern in lengthSpikes"
  comph' m x hh@(h:hs) = do
    new <- eulerStep x
    let hhh | v>=th        =  (h+1):hs
            | v<th && h==0 = hh
            | v<th && h>0  = 0:hh
            | otherwise = error "unexpected pattern in lengthSpikes"
            where v = varV new
    if m==1 then return (clean hhh) else comph' (m-1) new hhh
       where clean yy@ys = if head ys == 0 then ys else yy

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
    if length hhh == n then return (clean hhh) else comph' new hhh
       where clean yy@(ys) = if  head ys ==0 then ys else yy
