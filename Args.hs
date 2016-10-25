{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}

module Args (
  Args(..)
, runWithArgs
) where


import           Data.Monoid
import           Options.Applicative


data Custom = Custom1 | Custom2 deriving (Eq, Show, Ord)


data Args = Args { argVerbose :: Bool
                 , argCustom :: Custom
                 } deriving (Show)


parseArgs :: Parser Args
parseArgs = Args
     <$> switch
         ( long "verbose"
        <> short 'v'
        <> help "Be verbose.")
     <*> option parseCustomArg
         ( long "custom-arg"
        <> short 'c'
        <> value Custom1
        <> showDefault
        <> help "Arg with a custom parser." )


parseCustomArg :: ReadM Custom
parseCustomArg = eitherReader $ \s ->
  case s
    of "c1" -> Right Custom1
       "c2" -> Right Custom2
       x    -> Left $ "Failed to parse custom argument " <> x <> ", expected 'c1' or 'c2'"


runWithArgs:: (Args -> IO ())
           -> IO ()
runWithArgs rwa = execParser opts >>= rwa
  where
    opts = info (helper <*> parseArgs)
      ( fullDesc
     <> header "Add program header here."
     <> progDesc "Add program description here")

