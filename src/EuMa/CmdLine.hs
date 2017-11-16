module EuMa.CmdLine where

import Options.Applicative
import Data.Semigroup ((<>))

import EuMa.Types

parseCmdLine :: IO Options
parseCmdLine = execParser $
  info (optionsParser <**> helper) fullDesc


data Options = Options
  {
    optGlobals :: Global
  , optCommand :: Command
  }

data Command
  = Peaks Parameters
  | Curves Parameters
  | MultiCurves {
      multiNumCurves :: Int
    , multiParams :: Parameters }

globalParser :: Parser Global
globalParser = Global <$>
     option auto ( long "stepSize" <> value step)
 <*> option auto ( long "simTime" <> value time)
 <*> option auto ( long "totalSteps" <> value steps)
 <*> option auto ( long "totalSpikes" <> value 100)
 where
    step = 0.01
    time = 5000.0
    steps = (floor $ time/step)

optionsParser :: Parser Options
optionsParser = Options <$> globalParser <*> cmdParser

cmdParser :: Parser Command
cmdParser = hsubparser
  ( command "peaks" (info (Peaks <$> parametersParser) (progDesc "Generate peaks CSV"))
 <> command "curves" (info (Curves <$> parametersParser) (progDesc "Generate curves CSV"))
 <> command "multi" (info (MultiCurves <$> option auto ( long "numCurves") <*> parametersParser) (progDesc "Generate multiple peak curves"))
  )

parametersParser :: Parser Parameters
parametersParser = Parameters <$>
     option auto (long "cm" <> value 10 <> help  "(pF) Membrane capacitance")
  <*> option auto (long "gcal" <> value 2 <> help "(nS) Maximal conductance of Ca^2+ channels")
  <*> option auto (long "vca" <> value 60 <> help "(mV) Reversal potential for Ca^2+ channels")
  <*> option auto (long "vm" <> value (-20) <> help "(mV) Voltage value at midpoint of m_\\inf")
  <*> option auto (long "sm" <> value 12  <> help "(mV) Slope parameter of m_\\inf")
  <*> option auto (long "gk" <> value 3.2 <> help "(nS) Maximal conductance of K channels")
  <*> option auto (long "vk" <> value (-75) <> help "(mV) Reversal potential for K^+")
  <*> option auto (long "vn" <> value (-5) <> help "(mV) Voltage value at midpoint of n_\\inf")
  <*> option auto (long "sn" <> value 10  <> help "(mV) Slope parameter of n_\\inf")
  <*> option auto (long "taun" <> value 30 <> help "(ms) Time constant of n")
  <*> option auto (long "gsk" <> value 2  <> help "(nS) Maximal conductance of SK channels")
  <*> option auto (long "ks" <> value 0.4 <> help "(\\mu M) [Ca] at midpoint of S_\\inf")
  <*> option auto (long "gbk" <> value 0.5 <> help "(nS) Maximal conductance of BK channels")
  <*> option auto (long "vf" <> value (-20) <> help "(mV) Voltage value at midpoint of f_\\inf")
  <*> option auto (long "sf" <> value 1   <> help "(mV) Slope parameter of f_\\inf")
  <*> option auto (long "taubk" <> value 5 <> help "(ms) Time constant of f")
  <*> option auto (long "gl" <> value 0.2 <> help "(nS) Leak conductance")
  <*> option auto (long "vl" <> value (-50) <> help "(mV) Reversal potential for the leak current")
  <*> option auto (long "noise" <> value 4 <> help "(pA) Amplitude of noise current")
  <*> option auto (long "fc" <> value 0.01 <> help "(0.01) Fraction of free Ca^2+ions in cytoplasm")
  <*> pure 0.0015 -- option auto (long "alpha" <> value 0.0015 <> help "(\\mu M fC^-1) Conversion from charges to molar concentration")
  <*> option auto (long "kc" <> value 0.12 <> help "(ms^-1) Rate of Ca^2+ extrusion")
