{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}

module Args (
  Args(..)
, Cmd(..)
, runWithArgs
) where


import           Control.Monad
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Development.GitRev
import           Network.AWS.Data
import           Network.AWS.Types
import           Options.Applicative
import           System.Exit

data Cmd = CmdGet | CmdPut deriving (Eq, Show)

data Args = Args { argsVerbose :: !Bool
                 , argsVersion :: !Bool
                 , argsAwsProfile :: !(Maybe Text)
                 , argsRegion :: !(Maybe Region)
                 , argsKmsKey :: !Text
                 , argsS3Uri :: !String
                 , argsFileName :: !(Maybe FilePath)
                 , argsCmd :: !Cmd
                 } deriving (Show)


parseArgs :: Parser Args
parseArgs = Args
     <$> switch
         ( long "verbose"
        <> short 'v'
        <> help "Be verbose.")

     <*> switch
         ( long "version"
        <> short 'V'
        <> help "Print version and exit.")

     <*> (optional $ fmap T.pack $ strOption
         ( long "aws-profile"
        <> short 'p'
        <> help "AWS profile. Default config entry will be used if not given." ))

     <*> (optional $ option parseRegion
         ( long "region"
        <> short 'r'
        <> help "AWS region." ))

     <*> (fmap T.pack $ strOption
         ( long "kms-key"
        <> short 'k'
        <> metavar "KEY_ARN"
        <> help "KMS master key, e.g. arn:aws:kms:us-east-1:123456789012:alias/testkey, see a list of keys in IAM console."))

     <*> strOption
         ( long "s3-uri"
        <> short 's'
        <> help "S3 URI" )

     <*> (optional $ strOption
         ( long "file"
        <> short 'f'
        <> help "Name of the local file" ))
     <*> subparser
         ( command "get"
           (info (pure CmdGet)
             ( progDesc "Get file from S3 and decrypt." ))
        <> command "put"
           (info (pure CmdPut)
           ( progDesc "Put file to S3 and encrypt." ))
         )


parseRegion :: ReadM Region
parseRegion = eitherReader (fromText . T.pack)


runWithArgs:: (Args -> IO ())
           -> IO ()
runWithArgs rwa = execParser opts >>= printVersion >>= rwa
  where
    opts = info (helper <*> parseArgs)
      ( fullDesc
     <> header "A utility to maintain KMS-encrypted files in S3."
     <> progDesc ("Encrypt/Upload a file to S3 or Download/Decrypt it."
                <> "Source: https://github.com/andreyk0/kms-s3"))

    printVersion args@Args{..} = do
      when argsVersion $ die $ "Version: " <> $(gitBranch) <> "@" <> $(gitHash)
      return args
