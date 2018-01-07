{-# LANGUAGE RecordWildCards #-}

module EuMa.Types where

import qualified Data.Vector as V
import Data.Csv (ToRecord(..), toField)
import Data.Word (Word32)
import Data.Vector (Vector)


data Parameters =
  Parameters { cm :: Double      -- (pF) Membrane capacitance
             , eca :: Double     -- (mV) Reversal potential for Ca^2+ channels
             , ek :: Double      -- (mV) Reversal potential for K^+
             , el :: Double      -- (mV) Reversal potential for the leak current
             , gca :: Double     -- (nS) Maximal Ca^2+ channel conductance 
             , vm :: Double      -- (mV) Half-maximal voltage for m_\inf
             , sm :: Double      -- (mV) Slope parameter for m_\inf
             , gk :: Double      -- (nS) Maximal delayed rectifier K^+ channel conductance
             , vn :: Double      -- (mV) Half-maximal voltage for n_\inf
             , sn :: Double      -- (mV) Slope parameter for n_\inf
             , taun :: Double    -- (ms) Time constant of n
             , gsk :: Double     -- (nS) Maximal SK channel conductance 
             , ks :: Double      -- (\mu M) Half-maximal Ca^2+ for S_\inf
             , gkir :: Double    -- (nS) Maximal inward rectifier K^+ channel conductance
             , vk :: Double      -- (mV) Half-maximal voltage for k_\inf
             , sk :: Double      -- (mV) Slope parameter for k_\inf
             , gbk :: Double     -- (nS) Maximal BK channel conductance
             , vb :: Double      -- (mV) Half-maximal voltage for b_\inf
             , sb :: Double      -- (mV) Slope parameter for b_\inf
             , taubk :: Double   -- (ms) Time constant of f
             , ga :: Double      -- (nS) Maximal A-type channel conductance
             , va :: Double      -- (mV) Half-maximal voltage for a_\inf
             , sa :: Double      -- (mV) Slope parameter for a_\inf
             , vh :: Double      -- (mV) Half-maximal voltage for h_\inf
             , sh :: Double      -- (mV) Slope parameter for h_\inf
             , tauh :: Double    -- (ms) Time constant of h
             , gl :: Double      -- (nS) Maximal leak conductance
             , fc :: Double      -- Fraction of free cytosolic Ca^2+
             , alpha :: Double   -- (\mu M fC^-1) Conversion from charges to concentration
             , kc :: Double      -- (ms^-1) Rate of Ca^2+ extrusion
             , noise :: Double   -- (pA) Amplitude of noise current
             } deriving (Show)

instance ToRecord Parameters where
  toRecord Parameters{..} = --fixme add other parameters, and use ToNamedRecord instead
    V.fromList $ map toField [cm,gca,eca,vm,sm]

data Variables a = Variables { varV  :: !a -- membrane potential
                             , varn  :: !a -- activation of I_K
                             , varb  :: !a -- activation of I_BK
                             , varh  :: !a -- activation of I_A
                             , varCa :: !a -- intracellular Ca^2+ concentration
                             } deriving Show

instance Functor Variables where
  fmap g t = Variables { varV  = g (varV t)
                       , varn  = g (varn t)
                       , varb  = g (varb t)
                       , varh  = g (varh t)
                       , varCa = g (varCa t)}

instance Applicative Variables where
  pure x = Variables x x x x x
  f <*> v = Variables { varV  = (varV  f) (varV  v)
                      , varn  = (varn  f) (varn  v)
                      , varb  = (varb  f) (varb  v)
                      , varh  = (varh  f) (varh  v)
                      , varCa = (varCa f) (varCa v) }

type RandomSeed = Vector Word32

data Global = Global { stepSize    :: Double
                     , simTime     :: Double
                     , totalSpikes :: Int
                     , numThreads :: Maybe Int
                     , rndSeed :: Maybe RandomSeed
                     } deriving (Show)

totalSteps :: Global -> Int
totalSteps Global{..} = floor $ simTime / stepSize

-- model is silent when (max V - min V) < 10 mV
data Features = Silent      { meanV :: Double
                            , stdV :: Double
                            , minV :: Double
                            , maxV :: Double }       |
                Oscillating { duration :: [Double]  -- duration of active phase
                            , pptime :: [Double]    -- time since last peak
                            , amplitude :: [Double] -- (fixme TBD!) max V - min V
                            , area :: [Double]      -- (fixme TBD!) area under V cruve and threashold
                            , nlocmax :: [Double] } -- (fixme TBD!) number of local maxima
