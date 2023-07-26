module ParseArgs (Options (..), options) where

import Options.Applicative

data Options = Options
  { importReport :: String,
    owner :: String,
    repo :: String,
    ref :: String,
    preview :: Bool,
    publish :: Bool
  }

options :: Parser Options
options =
  Options
    <$> argument
      str
      ( metavar "Import Report"
          <> help "path to import report"
      )
    <*> argument
      str
      ( metavar "owner"
          <> help "Owner of the project repo"
      )
    <*> argument
      str
      ( metavar "repo"
          <> help "Name of the project repo"
      )
    <*> argument
      str
      ( metavar "ref"
          <> help "Which branch of the repo to look in"
      )
    <*> switch
      (long "preview" <> help "Preview all valid paths in the provided import report")
    <*> switch
      (long "publish" <> help "Publish all valid paths in the provided import report")
