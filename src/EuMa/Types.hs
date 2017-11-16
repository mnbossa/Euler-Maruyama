{-# LANGUAGE DeriveDataTypeable #-} -- for CmdArgs

module EuMa.Types where

import System.Console.CmdArgs

data Parameters =
  Parameters { filename :: String
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

-------------- Global (simulation) parameters --------------------------------------------------------
data Global = Global { stepSize    :: Double
                     , simTime     :: Double
                     , totalSteps  :: Int
                     , totalSpikes :: Int
                     } --deriving (Data,Typeable,Show,Eq)
