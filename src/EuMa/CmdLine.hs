module EuMa.CmdLine
  ( Options(..)
  , Command (..)
  , parseCmdLine
  , stringsToOptions)

where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Vector (singleton)

import EuMa.Types

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

-- parse a single number as a RandomSeed
seedParser :: Parser RandomSeed
seedParser = singleton <$> option auto (long "rndSeed")

globalParser :: Parser Global
globalParser = Global <$>
     option auto (long "stepSize" <> value step)
 <*> option auto (long "simTime" <> value time)
 <*> option auto (long "totalSteps" <> value steps)
 <*> option auto (long "totalSpikes" <> value 100)
 <*> optional (option auto (long "numThreads"))
 <*> (fmap Just seedParser <|> pure Nothing)
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
      option auto (long "Cm"   <> value 10    <> help "( 10 pF) Membrane capacitance")
  <*> option auto (long "ECa"  <> value 60    <> help "( 60 mV) Reversal potential for Ca^2+ channels")
  <*> option auto (long "EK"   <> value (-75) <> help "(-75 mV) Reversal potential for K^+")
  <*> option auto (long "EL"   <> value (-50) <> help "(-50 mV) Reversal potential for the leak current")
  <*> option auto (long "gCa"  <> value 2     <> help "(  2 nS) Maximal Ca^2+ channel conductance ")
  <*> option auto (long "Vm"   <> value (-20) <> help "(-20 mV) Half-maximal voltage for m_\\inf")
  <*> option auto (long "sm"   <> value 12    <> help "( 12 mV) Slope parameter for m_\\inf")
  <*> option auto (long "gK"   <> value 3.2   <> help "(3.2 nS) Maximal delayed rectifier K^+ channel conductance")
  <*> option auto (long "Vn"   <> value (-5)  <> help "( -5 mV) Half-maximal voltage for n_\\inf")
  <*> option auto (long "sn"   <> value 10    <> help "( 10 mV) Slope parameter for n_\\inf")
  <*> option auto (long "taun" <> value 30    <> help "( 30 ms) Time constant of n")
  <*> option auto (long "gSK"  <> value 2     <> help "(  2 nS) Maximal SK channel conductance ")
  <*> option auto (long "ks"   <> value 0.4   <> help "(0.4 \\mu M) Half-maximal Ca^2+ for S_\\inf")
  <*> option auto (long "gKir" <> value 0     <> help "(  0 nS) Maximal inward rectifier K^+ channel conductance")
  <*> option auto (long "Vk"   <> value (-65) <> help "(-65 mV) Half-maximal voltage for k_\\inf")
  <*> option auto (long "sk"   <> value (-8)  <> help "( -8 mV) Slope parameter for k_\\inf")
  <*> option auto (long "gBK"  <> value 0     <> help "(  0 nS) Maximal BK channel conductance ")
  <*> option auto (long "Vb"   <> value (-20) <> help "(-20 mV) Half-maximal voltage for b_\\inf")
  <*> option auto (long "sb"   <> value 2     <> help "(  2 mV) Slope parameter for b_\\inf")
  <*> option auto (long "tauBK"<> value 5     <> help "(  5 ms) Time constant of b")
  <*> option auto (long "gA"   <> value 0     <> help "(  0 nS) Maximal A-type channel conductance")
  <*> option auto (long "Va"   <> value (-20) <> help "(-20 mV) Half-maximal voltage for a_\\inf")
  <*> option auto (long "sa"   <> value 10    <> help "( 10 mV) Slope parameter for a_\\inf")
  <*> option auto (long "Vh"   <> value (-60) <> help "(-60 mV) Half-maximal voltage for h_\\inf")
  <*> option auto (long "sh"   <> value (-5)  <> help "( -5 mV) Slope parameter for h_\\inf")
  <*> option auto (long "tauh" <> value 20    <> help "( 20 ms) Time constant of h")
  <*> option auto (long "gL"   <> value 0.2   <> help "(0.2 nS) Maximal leak conductance")
  <*> option auto (long "fc"   <> value 0.01  <> help "(0.01) Fraction of free cytosolic Ca^2+")
  <*> pure 0.0015 -- option auto (long "alpha" <> value 0.0015 <> help "(\\mu M fC^-1) Conversion from charges to concentration")
  <*> option auto (long "kc"   <> value 0.12 <> help "(0.12 ms^-1) Rate of Ca^2+ extrusion")
  <*> option auto (long "noise" <> value 4 <> help "(  4 pA) Amplitude of noise current")

stringsToOptions :: [String] -> Maybe Options
stringsToOptions ss =
  case execParserPure defaultPrefs parserInfo ss of
    Success options -> Just options
    _ -> Nothing

parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper) fullDesc

parseCmdLine :: IO Options
parseCmdLine = execParser parserInfo




-- fixme use this
-- helpText :: String
-- helpText = "\
-- \Generate predictions from a stochastic model for the activity of a pituitary \
-- \lactotroph as described in the paper Fast-Activating Voltage- and \
-- \Calcium-Dependent Potassium (BK) Conductance Promotes Bursting in Pituitary \
-- \Cells, J. Tabak, M. Tomaiuolo, A.E. Gonzalez-Iglesias, L.S. Milescu, R. \
-- \Bertram, the Journal of Neuroscience, 31:16855-16863, 2011. \n\
-- \\n  Usage:\n    ./pituitary file [OPTIONS]     \n   Produce 4 .png files \
-- \(the images of each variable trajectory vs. time) and a .txt file (spike \
-- \lengths: number of time-step) using a fixed simulation time.\n \n \n\
-- \   ./pituitary file.fig [OPTIONS] \n Produce 4 .fig files as above, \
-- \but where fig is one of the following: png, pdf, jpg, jpeg. No txt \
-- \file is produced.\n \n    ./pituitary file.txt [OPTIONS] \n \
-- \ Produce only txt file. Simulation is run until a fixed number of spikes \
-- \is obtained."
