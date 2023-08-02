module ParseArgs (Options (..), Actions (..), options) where

import Options.Applicative

data Actions
  = Actions
      { pre :: Bool,
        pub :: Bool
      }
  | Delete
  | Unpublish
  deriving (Show)

data Options = Options
  { fil :: String,
    own :: String,
    rep :: String,
    re :: String,
    acts :: Actions
  }
  deriving (Show)

options :: Parser Options
options =
  Options
    <$> argument
      str
      ( metavar "path/to/file"
          <> help "Path to the import report excel file or text file containing the paths delimited by newlines"
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
    <*> act

act :: Parser Actions
act = create <|> unpublish <|> delete

create :: Parser Actions
create =
  Actions
    <$> switch
      (long "preview" <> help "Preview all valid paths")
    <*> switch
      (long "publish" <> help "Publish all valid paths")

delete :: Parser Actions
delete =
  flag'
    Delete
    (long "delete" <> short 'd' <> help "Unpublish all given urls and delete the previews")

unpublish :: Parser Actions
unpublish =
  flag'
    Unpublish
    (long "unpublish" <> help "Unpublish all given urls")
