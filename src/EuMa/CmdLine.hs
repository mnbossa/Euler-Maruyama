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
cmdParser = subparser
  ( command "peaks" (info (Peaks <$> parametersParser) (progDesc "Generate peaks CSV"))
 <> command "curves" (info (Curves <$> parametersParser) (progDesc "Generate curves CSV"))
 <> command "multi" (info (MultiCurves <$> option auto ( long "numCurves") <*> parametersParser) (progDesc "Generate multiple peak curves"))
  )

parametersParser :: Parser Parameters
parametersParser = Parameters <$>
     option auto (long "cm" <> value 10)
  <*> option auto (long "gcal" <> value 2)
  <*> option auto (long "vca" <> value 60)
  <*> option auto (long "vm" <> value (-20))
  <*> option auto (long "sm" <> value 12)
  <*> option auto (long "gk" <> value 3.2)
  <*> option auto (long "vk" <> value (-75))
  <*> option auto (long "vn" <> value (-5))
  <*> option auto (long "sn" <> value 10)
  <*> option auto (long "taun" <> value 30)
  <*> option auto (long "gsk" <> value 2)
  <*> option auto (long "ks" <> value 0.4)
  <*> option auto (long "gbk" <> value 0.5)
  <*> option auto (long "vf" <> value (-20))
  <*> option auto (long "sf" <> value 1)
  <*> option auto (long "taubk" <> value 5)
  <*> option auto (long "gl" <> value 0.2)
  <*> option auto (long "vl" <> value (-50))
  <*> option auto (long "noise" <> value 4)
  <*> option auto (long "fc" <> value 0.01)
  <*> option auto (long "alpha" <> value 0.0015)
  <*> option auto (long "kc" <> value 0.12)
